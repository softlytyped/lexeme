open! Core
open Parsetree
open Binding_syntax
module Json = Yojson.Safe.Util

let query source command arguments =
  let open Or_error.Let_syntax in
  (* We query the source's editor configuration as an external Dune client, even when Dune
     launched the linter inside a larger workspace. *)
  let%bind output =
    Process.read
      ~input:(Source.text_of source)
      ~environment:[ "INSIDE_DUNE", None ]
      "ocamlmerlin"
      ([ "single"; command; "-filename"; Source.path_of source ] @ arguments)
  in
  Or_error.try_with (fun () ->
    let response = Yojson.Safe.from_string output in
    Json.member "value" response |> Json.to_list)
;;

let position_of position =
  Json.member "line" position |> Json.to_int, Json.member "col" position |> Json.to_int
;;

let coordinates_of (position : Lexing.position) =
  position.pos_lnum, position.pos_cnum - position.pos_bol
;;

type t =
  { source : Source.t
  ; annotations : (int * int * string, string) Hashtbl.Poly.t Or_error.t Lazy.t
  }

let types_of source =
  let annotations =
    lazy
      (let annotations = Hashtbl.Poly.create () in
       let rec visit node =
         (match
            Json.member "name" node, Json.member "type" node, Json.member "selection" node
          with
          | `String name, `String annotation, (`Assoc _ as selection) ->
            let line, column = position_of (Json.member "start" selection) in
            Hashtbl.set annotations ~key:(line, column, name) ~data:annotation
          | _ -> ());
         Json.member "children" node |> Json.to_list |> List.iter ~f:visit
       in
       let open Or_error.Let_syntax in
       let%bind nodes = query source "outline" [] in
       let%map () = Or_error.try_with (fun () -> List.iter nodes ~f:visit) in
       annotations)
  in
  { source; annotations }
;;

let parse text =
  Or_error.try_with (fun () -> Parse.core_type (Lexing.from_string text)) |> Result.ok
;;

let annotation_at typing (name : string Location.loc) =
  let line, column = coordinates_of name.loc.loc_start in
  let%bind.Option annotations = Result.ok (force typing.annotations) in
  Hashtbl.find annotations (line, column, name.txt) |> Option.bind ~f:parse
;;

let verify typing =
  match Stdlib.Lazy.is_val typing.annotations with
  | false -> Ok ()
  | true -> Or_error.map (force typing.annotations) ~f:ignore
;;

let rec is_function annotation =
  match annotation.ptyp_desc with
  | Ptyp_arrow _ -> true
  | Ptyp_poly (_, body) -> is_function body
  | _ -> false
;;

let rec codomain_of annotation =
  match annotation.ptyp_desc with
  | Ptyp_poly (_, body) -> codomain_of body
  | Ptyp_arrow _ ->
    (* OCaml and OxCaml visit the argument and result types of an arrow in that order. *)
    let codomain = ref annotation in
    let base = Ast_iterator.default_iterator in
    base.typ { base with typ = (fun _ child -> codomain := child) } annotation;
    codomain_of !codomain
  | _ -> annotation
;;

let constructor_of annotation =
  match annotation.ptyp_desc with
  | Ptyp_constr (path, arguments) ->
    Format.asprintf "%a" Pprintast.longident path.txt, arguments
  | _ -> "", []
;;

let public_path_of path =
  match String.lsplit2 path ~on:'.' with
  | Some (("Core" | "Base" | "Stdlib"), name) -> name
  | _ -> path
;;

let is_boolean annotation =
  let path, arguments = constructor_of annotation in
  List.is_empty arguments
  && List.mem [ "bool"; "Bool.t" ] (public_path_of path) ~equal:String.equal
;;

let result_of annotation =
  let codomain = codomain_of annotation in
  let path, arguments = constructor_of codomain in
  match public_path_of path, arguments with
  | _, [] when is_boolean codomain -> Boolean
  | ("option" | "Option.t"), [ value ] when is_boolean value -> Optional_boolean
  | "Or_error.t", [ value ] when is_boolean value -> Fallible_boolean
  | ("result" | "Result.t"), [ value; error ]
    when is_boolean value
         && String.equal (public_path_of (fst (constructor_of error))) "Error.t" ->
    Fallible_boolean
  | _ -> Other (Format.asprintf "%a" Pprintast.core_type codomain)
;;

let expansion_at typing (name : string Location.loc) ~extent =
  let line, column = coordinates_of name.loc.loc_start in
  let open Or_error.Let_syntax in
  let%bind nodes =
    query
      typing.source
      "type-enclosing"
      [ "-position"; sprintf "%d:%d" line column; "-verbosity"; "2" ]
  in
  let regions = name.loc :: Option.to_list extent in
  Or_error.try_with (fun () ->
    List.find_map nodes ~f:(fun node ->
      let start = position_of (Json.member "start" node) in
      let stop = position_of (Json.member "end" node) in
      let matches =
        List.exists regions ~f:(fun (region : Location.t) ->
          [%equal: int * int] start (coordinates_of region.loc_start)
          && [%equal: int * int] stop (coordinates_of region.loc_end))
      in
      match matches, Json.member "type" node with
      | true, `String text -> parse text
      | _ -> None))
;;

let result_at ?extent typing name ~annotation =
  let annotation =
    match annotation with
    | Some _ -> annotation
    | None -> annotation_at typing name
  in
  let result = Option.value_map annotation ~default:Unknown ~f:result_of in
  match result with
  | Boolean | Optional_boolean | Fallible_boolean -> result
  | Other _ | Unknown ->
    (match expansion_at typing name ~extent with
     | Ok (Some expansion) -> result_of expansion
     | _ -> result)
;;
