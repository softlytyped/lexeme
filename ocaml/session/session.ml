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
  ; owners : string Source.Extent.Map.t Or_error.t Lazy.t
  }

let extent_of (location : Ocaml_parsing.Location.t) =
  Source.Extent.create ~start:location.loc_start.pos_cnum ~stop:location.loc_end.pos_cnum
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
    else Other_type (spelling_of environment description)
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
let codomain_shape_of pipeline environment description ~errors =
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
    let shape = codomain_shape_of pipeline environment description ~errors in
    Map.set shapes ~key:(extent_of location) ~data:shape)
;;

(* We visit each node of the typed tree with the environment that holds at it. *)
let fold_over typing ~seed ~visit =
  let rec descend environment node state =
    let environment = Node.node_update_env environment node in
    Node.fold_node descend environment node (visit environment node state)
  in
  descend (Mtyper.initial_env typing) (root_of typing) seed
;;

let shapes_in pipeline typing =
  let errors = lazy (not (List.is_empty (Mtyper.get_errors typing))) in
  fold_over typing ~seed:Source.Extent.Map.empty ~visit:(fun environment node shapes ->
    Option.fold
      (declaration_of node)
      ~init:shapes
      ~f:(with_shape pipeline environment ~errors))
;;

(* A field's name, with the description of its label, where a program reads the field,
   writes it, builds a record with it, or matches it in a pattern. *)
let written_labels_of fields =
  Array.to_list fields
  |> List.filter_map
       ~f:(fun (label, _, (definition : Typedtree.record_label_definition)) ->
         match definition with
         | Overridden (lid, _) -> Some (lid.loc, label)
         | Kept _ -> None)
;;

let labels_of = function
  | Node.Expression { exp_desc = Texp_field { lid; label; _ }; _ }
  | Node.Expression { exp_desc = Texp_setfield { lid; label; _ }; _ } ->
    [ lid.loc, label ]
  | Node.Expression { exp_desc = Texp_record { fields; _ }; _ } ->
    written_labels_of fields
  | Node.Pattern { pat_desc = Tpat_record (fields, _, _); _ } ->
    List.map fields ~f:(fun (lid, label, _) -> lid.loc, label)
  | _ -> []
;;

(* An installed library lies beside the standard library, whose directory holds
   stdlib.cmi, so a module whose compiled interface lies elsewhere is the project's own. *)
let standard_interface = "stdlib.cmi"
let has_file directory file = Stdlib.Sys.file_exists (Filename.concat directory file)

let library_directory_of directories =
  List.find directories ~f:(fun directory -> has_file directory standard_interface)
  |> Option.map ~f:Filename.dirname
;;

let is_built_here ~build_path:directories name =
  let file = String.uncapitalize name ^ ".cmi" in
  let libraries = library_directory_of directories in
  let is_installed directory =
    Option.exists libraries ~f:(fun libraries ->
      String.is_prefix directory ~prefix:(Filename.concat libraries ""))
  in
  List.exists directories ~f:(fun directory ->
    has_file directory file && not (is_installed directory))
;;

(* The head of a path, such as Source in Source.Site.t, unless the path applies a functor,
   whose result no single module owns. *)
let rec head_of : Path.t -> Ident.t option = function
  | Pident identifier -> Some identifier
  | Pdot (path, _) -> head_of path
  | Papply _ | Pextra_ty _ -> None
;;

(* A type of another compilation unit has a global identifier at the head of its path,
   unlike a type of the current one. *)
let owner_of (label : Data_types.label_description) ~is_own =
  match Types.get_desc label.lbl_res with
  | Tconstr (path, _, _) ->
    let%bind.Option head = head_of path in
    if Ident.is_global head && is_own (Ident.name head)
    then Option.map (String.rsplit2 (Path.name path) ~on:'.') ~f:fst
    else None
  | _ -> None
;;

(* We ask once for each module whether the project builds it. *)
let owners_in pipeline typing =
  let build_path = Mconfig.build_path (Mpipeline.final_config pipeline) in
  let is_own = Memo.of_comparator (module String) (is_built_here ~build_path) in
  fold_over typing ~seed:Source.Extent.Map.empty ~visit:(fun _ node owners ->
    List.fold (labels_of node) ~init:owners ~f:(fun owners (location, label) ->
      match owner_of label ~is_own with
      | Some owner -> Map.set owners ~key:(extent_of location) ~data:owner
      | None -> owners))
;;

let typed_index_of pipeline ~path ~index =
  Or_error.try_with (fun () ->
    Mpipeline.with_pipeline pipeline (fun () -> index (Mpipeline.typer_result pipeline)))
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

let source_comment_of (body, location) =
  Source.Comment.create ~extent:(extent_of location) ~body
;;

let session_of pipeline ~path =
  let%map.Or_error syntax_tree = parse pipeline in
  { pipeline
  ; syntax_tree
  ; comments = List.map (Mpipeline.reader_comments pipeline) ~f:source_comment_of
  ; shapes = lazy (typed_index_of pipeline ~path ~index:(shapes_in pipeline))
  ; owners = lazy (typed_index_of pipeline ~path ~index:(owners_in pipeline))
  }
;;

let create ~path text =
  let%bind.Or_error pipeline = Or_error.try_with (fun () -> pipeline_of ~path text) in
  Or_error.try_with_join (fun () ->
    Mpipeline.with_pipeline pipeline (fun () -> session_of pipeline ~path))
;;

let syntax_tree_of session = session.syntax_tree
let comments_of session = session.comments

let owner_at session extent =
  let%bind.Option owners = Result.ok (force session.owners) in
  Map.find owners extent
;;

let shape_at session extent =
  let%bind.Option shapes = Result.ok (force session.shapes) in
  Map.find shapes extent
;;

let verify session =
  let check index =
    if Lazy.is_val index then Or_error.ignore_m (force index) else Ok ()
  in
  Or_error.combine_errors_unit [ check session.shapes; check session.owners ]
;;
