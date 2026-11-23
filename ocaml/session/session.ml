open! Core
open Merlin_kernel
open Ocaml_typing
module Codomain = Binding.Codomain
module Node = Merlin_specific.Browse_raw

(* A type is a value's, or a function's with its final result, beneath any quantifier. *)
module Shape = struct
  type 'codomain t =
    | Value of 'codomain
    | Function of 'codomain

  let result_of = function
    | Value result | Function result -> result
  ;;

  let map shape ~f:transform =
    match shape with
    | Value result -> Value (transform result)
    | Function result -> Function (transform result)
  ;;

  let rec of_type environment description =
    let description = Ctype.expand_head environment description in
    match Types.get_desc description with
    | Tpoly (body, _) -> of_type environment body
    | Tarrow (_, _, body, _) -> Function (result_of (of_type environment body))
    | _ -> Value description
  ;;
end

type t =
  { pipeline : Mpipeline.t
  ; syntax_tree : Mreader.parsetree
  ; comments : Source.Comment.t list
  ; shapes : Codomain.t Lazy.t Shape.t Source.Extent.Map.t Or_error.t Lazy.t
  }

let extent_of (location : Ocaml_parsing.Location.t) =
  { Source.Extent.start = location.loc_start.pos_cnum; stop = location.loc_end.pos_cnum }
;;

(* Base's and Core's fallible Booleans, where the environment knows those libraries. *)
let fallible_booleans_in environment =
  List.filter_map [ "Base"; "Core" ] ~f:(fun library ->
    let%bind.Option path =
      Ocaml_parsing.Longident.unflatten [ library; "Or_error"; "t" ]
    in
    match Env.find_type_by_name path environment with
    | path, _ ->
      Some (Ctype.newconstr path [ Predef.type_bool ], Codomain.Fallible_boolean)
    | exception Stdlib.Not_found -> None)
;;

let expectations_in environment =
  [ Predef.type_bool, Codomain.Boolean
  ; Predef.type_unit, Unit "unit"
  ; Predef.type_option Predef.type_bool, Optional_boolean
  ]
  @ fallible_booleans_in environment
;;

(* A type variable in a source that does not type stands for a type the compiler could not
   find. *)
let is_undetermined environment description ~errors =
  (not (Ctype.closed_type_expr ~env:environment description)) && force errors
;;

let spelling_of environment description =
  Printtyp.wrap_printing_env environment (fun () ->
    Format.asprintf "%a" Printtyp.type_scheme description)
;;

let codomain_of environment description ~errors =
  let is_expected (expectation, _) =
    Ctype.is_equal environment false [ description ] [ expectation ]
  in
  match List.find (expectations_in environment) ~f:is_expected with
  | Some (_, codomain) -> codomain
  | None ->
    if is_undetermined environment description ~errors
    then Unknown
    else Other (spelling_of environment description)
;;

let method_of = function
  | Node.Class_field { cf_desc = Tcf_method (name, _, Tcfk_concrete (_, body)); _ } ->
    Some (name.loc, body.exp_type)
  | Node.Class_field { cf_desc = Tcf_method (name, _, Tcfk_virtual annotation); _ } ->
    Some (name.loc, annotation.ctyp_type)
  | Node.Class_type_field
      { ctf_desc = Tctf_method (_, _, _, annotation); ctf_loc = site; _ } ->
    Some (site, annotation.ctyp_type)
  | _ -> None
;;

let declaration_of = function
  | Node.Pattern pattern -> Some (pattern.pat_loc, pattern.pat_type)
  | Node.Value_description value -> Some (value.val_name.loc, value.val_desc.ctyp_type)
  | Node.Label_declaration label -> Some (label.ld_name.loc, label.ld_type.ctyp_type)
  | node -> method_of node
;;

(* The caller may judge a binding after another file has been read, so the codomain
   restores the compiler environment that owns its types. *)
let shape_in pipeline environment description ~errors =
  Shape.map (Shape.of_type environment description) ~f:(fun result ->
    lazy
      (Mpipeline.with_pipeline pipeline (fun () -> codomain_of environment result ~errors)))
;;

let root_of typing =
  match Mtyper.get_typedtree typing with
  | `Interface signature -> Node.Signature signature
  | `Implementation structure -> Node.Structure structure
;;

(* Where the source does not type, we leave an undetermined type to the written syntax. *)
let with_shape pipeline environment ~errors shapes (location, description) =
  if is_undetermined environment description ~errors
  then shapes
  else (
    let shape = shape_in pipeline environment description ~errors in
    Map.set shapes ~key:(extent_of location) ~data:shape)
;;

let index_of pipeline typing =
  let errors = lazy (not (List.is_empty (Mtyper.get_errors typing))) in
  let rec visit environment node shapes =
    let environment = Node.node_update_env environment node in
    let add = with_shape pipeline environment ~errors in
    let shapes = Option.fold (declaration_of node) ~init:shapes ~f:add in
    Node.fold_node visit environment node shapes
  in
  visit (Mtyper.initial_env typing) (root_of typing) Source.Extent.Map.empty
;;

let shapes_of pipeline ~path =
  Or_error.try_with (fun () ->
    Mpipeline.with_pipeline pipeline (fun () ->
      index_of pipeline (Mpipeline.typer_result pipeline)))
  |> Or_error.tag ~tag:("could not read the compiler types for " ^ path)
;;

let pipeline_of ~path text =
  let initial = Mconfig.initial in
  let query =
    { initial.query with
      directory = Filename.dirname path
    ; filename = Filename.basename path
    }
  in
  let configuration = Mconfig.get_external_config path { initial with query } in
  Mpipeline.make configuration (Msource.make text)
;;

let parse pipeline =
  match
    Mpipeline.reader_lexer_errors pipeline @ Mpipeline.reader_parser_errors pipeline
  with
  | [] -> Ok (Mpipeline.reader_parsetree pipeline)
  | errors ->
    List.map errors ~f:(Format.asprintf "%a" Ocaml_parsing.Location.report_exception)
    |> String.concat ~sep:"\n"
    |> Or_error.error_string
;;

let session_of pipeline ~path =
  let comment_of (body, location) =
    { Source.Comment.extent = extent_of location; body }
  in
  let%map.Or_error syntax_tree = parse pipeline in
  { pipeline
  ; syntax_tree
  ; comments = List.map (Mpipeline.reader_comments pipeline) ~f:comment_of
  ; shapes = lazy (shapes_of pipeline ~path)
  }
;;

let create ~path text =
  let%bind.Or_error pipeline = Or_error.try_with (fun () -> pipeline_of ~path text) in
  Or_error.try_with_join (fun () ->
    Mpipeline.with_pipeline pipeline (fun () -> session_of pipeline ~path))
;;

let syntax_tree_of session = session.syntax_tree
let comments_of session = session.comments

let shape_at session extent =
  let%bind.Option shapes = Result.ok (force session.shapes) in
  Map.find shapes extent
;;

let verify session =
  if Lazy.is_val session.shapes then Or_error.ignore_m (force session.shapes) else Ok ()
;;
