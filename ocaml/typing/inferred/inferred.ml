open! Core
open Binding_syntax
open Merlin_kernel
open Ocaml_typing
module Node = Merlin_specific.Browse_raw

type t = (int * int, role) Map.Poly.t Or_error.t Lazy.t

let is_fallible environment description =
  List.exists [ "Base"; "Core" ] ~f:(fun library ->
    let path = Ocaml_parsing.Longident.unflatten [ library; "Or_error"; "t" ] in
    Option.exists path ~f:(fun path ->
      match Env.find_type_by_name path environment with
      | path, _ ->
        let expectation = Ctype.newconstr path [ Predef.type_bool ] in
        Ctype.is_equal environment false [ description ] [ expectation ]
      | exception Stdlib.Not_found -> false))
;;

let result_of environment description ~errors =
  let is_equal expectation =
    Ctype.is_equal environment false [ description ] [ expectation ]
  in
  match Types.get_desc description with
  | _ when is_equal Predef.type_bool -> Boolean
  | _ when is_equal (Predef.type_option Predef.type_bool) -> Optional_boolean
  | Tconstr (_, [ _; _ ], _) when is_fallible environment description -> Fallible_boolean
  | _ when (not (Ctype.closed_type_expr ~env:environment description)) && force errors ->
    Unknown
  | _ ->
    Other
      (Printtyp.wrap_printing_env environment (fun () ->
         Format.asprintf "%a" Printtyp.type_scheme description))
;;

let rec codomain_of environment description =
  let description = Ctype.expand_head environment description in
  match Types.get_desc description with
  | Tpoly (body, _) -> codomain_of environment body
  | Tarrow (_, _, body, _) ->
    let _, result = codomain_of environment body in
    true, result
  | _ -> false, description
;;

type declaration =
  | Value
  | Method

let declaration_of = function
  | Node.Pattern pattern -> Some (Value, pattern.pat_loc, pattern.pat_type)
  | Node.Value_description value ->
    Some (Value, value.val_name.loc, value.val_desc.ctyp_type)
  | Node.Label_declaration label ->
    Some (Value, label.ld_name.loc, label.ld_type.ctyp_type)
  | Node.Class_field { cf_desc = Tcf_method (name, _, Tcfk_concrete (_, body)); _ } ->
    Some (Method, name.loc, body.exp_type)
  | Node.Class_field { cf_desc = Tcf_method (name, _, Tcfk_virtual annotation); _ } ->
    Some (Method, name.loc, annotation.ctyp_type)
  | Node.Class_type_field
      { ctf_desc = Tctf_method (_, _, _, annotation); ctf_loc = location; _ } ->
    Some (Method, location, annotation.ctyp_type)
  | _ -> None
;;

let fold pipeline typing =
  let errors = lazy (not (List.is_empty (Mtyper.get_errors typing))) in
  let rec visit environment node types =
    let environment = Node.node_update_env environment node in
    let types =
      match declaration_of node with
      | None -> types
      | Some (declaration, location, description) ->
        let arrow, result = codomain_of environment description in
        let role =
          match declaration, arrow with
          | Value, false -> Variable
          | Method, _ | Value, true ->
            (* The caller may inspect a predicate after another file has been read. We
               restore the compiler environment that owns its types. *)
            Function
              (lazy
                (Mpipeline.with_pipeline pipeline (fun () ->
                   result_of environment result ~errors)))
        in
        let extent = location.loc_start.pos_cnum, location.loc_end.pos_cnum in
        Map.set types ~key:extent ~data:role
    in
    Node.fold_node visit environment node types
  in
  let root =
    match Mtyper.get_typedtree typing with
    | `Interface signature -> Node.Signature signature
    | `Implementation structure -> Node.Structure structure
  in
  visit (Mtyper.initial_env typing) root Map.Poly.empty
;;

let read source =
  let path = Source.path_of source in
  let open Or_error.Let_syntax in
  let%bind pipeline =
    Or_error.try_with (fun () ->
      let initial = Mconfig.initial in
      let query =
        { initial.query with
          directory = Filename.dirname path
        ; filename = Filename.basename path
        }
      in
      let configuration = Mconfig.get_external_config path { initial with query } in
      Mpipeline.make configuration (Msource.make (Source.text_of source)))
  in
  let%bind syntax =
    Or_error.try_with_join (fun () ->
      Mpipeline.with_pipeline pipeline (fun () ->
        match
          Mpipeline.reader_lexer_errors pipeline @ Mpipeline.reader_parser_errors pipeline
        with
        | [] -> Ok (Mpipeline.reader_parsetree pipeline)
        | errors ->
          Or_error.error_string
            (List.map
               errors
               ~f:(Format.asprintf "%a" Ocaml_parsing.Location.report_exception)
             |> String.concat ~sep:"\n")))
  in
  let types =
    lazy
      (Or_error.try_with (fun () ->
         Mpipeline.with_pipeline pipeline (fun () ->
           fold pipeline (Mpipeline.typer_result pipeline)))
       |> Or_error.tag ~tag:("could not read the compiler types for " ^ path))
  in
  Ok (syntax, types)
;;

let role_at types ~start ~stop =
  let%bind.Option index = Result.ok (force types) in
  Map.find index (start, stop)
;;

let verify types =
  match Stdlib.Lazy.is_val types with
  | false -> Ok ()
  | true -> Or_error.map (force types) ~f:ignore
;;
