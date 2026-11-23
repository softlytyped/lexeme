open! Core
open Syntax.Let_syntax
module Codomain = Binding.Codomain

(* We follow C's lexical states only as far as comments need them, since a string or a
   character constant can hold a comment's delimiters. *)
let line_opening = "//"
let block_opening = "/*"
let block_closing = "*/"

let character_at text offset =
  match offset < String.length text with
  | true -> Some text.[offset]
  | false -> None
;;

let comment_of text ~start ~stop ~closing =
  let body =
    String.slice text (start + String.length block_opening) (stop - String.length closing)
  in
  { Source.Comment.extent = { start; stop }; body }
;;

let rec scan_code text offset comments =
  let is_at opening = String.is_substring_at text ~pos:offset ~substring:opening in
  match character_at text offset with
  | None -> List.rev comments
  | Some '/' when is_at line_opening ->
    scan_line_comment text ~start:offset (offset + String.length line_opening) comments
  | Some '/' when is_at block_opening ->
    scan_block_comment text ~start:offset (offset + String.length block_opening) comments
  | Some (('"' | '\'') as quote) -> scan_quotation text ~quote (offset + 1) comments
  | Some _ -> scan_code text (offset + 1) comments

and scan_quotation text ~quote offset comments =
  match character_at text offset with
  | None -> List.rev comments
  | Some '\\' -> scan_escape text ~quote (offset + 1) comments
  | Some letter when Char.equal letter quote || Char.equal letter '\n' ->
    scan_code text (offset + 1) comments
  | Some _ -> scan_quotation text ~quote (offset + 1) comments

and scan_escape text ~quote offset comments =
  scan_quotation text ~quote (offset + 1) comments

(* A backslash at the end of a line continues a line comment onto the next. *)
and scan_line_comment text ~start offset comments =
  let close () = comment_of text ~start ~stop:offset ~closing:"" :: comments in
  match character_at text offset with
  | None -> List.rev (close ())
  | Some '\n' when not (Char.equal text.[offset - 1] '\\') ->
    scan_code text offset (close ())
  | Some _ -> scan_line_comment text ~start (offset + 1) comments

and scan_block_comment text ~start offset comments =
  let stop = offset + String.length block_closing in
  match character_at text offset with
  | None -> List.rev (comment_of text ~start ~stop:offset ~closing:"" :: comments)
  | Some '*' when String.is_substring_at text ~pos:offset ~substring:block_closing ->
    scan_code text stop (comment_of text ~start ~stop ~closing:block_closing :: comments)
  | Some _ -> scan_block_comment text ~start (offset + 1) comments
;;

(* Clang's declaration tree is JSON. *)
let field_of node name =
  match node with
  | `Assoc fields -> List.Assoc.find fields name ~equal:String.equal
  | _ -> None
;;

let text_of node name =
  match field_of node name with
  | Some (`String text) -> Some text
  | _ -> None
;;

let children_of node =
  match field_of node "inner" with
  | Some (`List children) -> children
  | _ -> []
;;

(* Clang writes a location's fields at the location, or at the expansion of the macro that
   spells it. *)
let rec integer_at location ~name =
  match field_of location name with
  | Some (`Int value) -> Some value
  | _ -> Option.bind (field_of location "expansionLoc") ~f:(integer_at ~name)
;;

let extent_of node =
  let open Option.Let_syntax in
  let%bind range = field_of node "range" in
  let%bind start = Option.bind (field_of range "begin") ~f:(integer_at ~name:"offset") in
  let%bind finish = field_of range "end" in
  let%map last = integer_at finish ~name:"offset"
  and length = integer_at finish ~name:"tokLen" in
  { Source.Extent.start; stop = last + length }
;;

(* We recognise a header's declarations by their include site, even where Clang elides the
   file's name. A macro expansion carries a spelling and an expansion location. *)
let is_spelled_here node =
  let location = Option.value (field_of node "loc") ~default:`Null in
  Option.is_none (field_of location "includedFrom")
  && Option.is_none (field_of location "expansionLoc")
;;

let spelling_of node =
  let%bind.Option description = field_of node "type" in
  Option.first_some
    (text_of description "desugaredQualType")
    (text_of description "qualType")
;;

let alias_after aliases node =
  match text_of node "kind", text_of node "name", spelling_of node with
  | Some "TypedefDecl", Some name, Some spelling ->
    Map.set aliases ~key:name ~data:spelling
  | _ -> aliases
;;

let rec expand aliases ~visited spelling =
  match Map.find aliases spelling with
  | Some target when not (Set.mem visited spelling) ->
    expand aliases ~visited:(Set.add visited spelling) target
  | _ -> spelling
;;

let codomain_of_result result =
  let is_qualifier = function
    | "const" | "volatile" -> true
    | _ -> false
  in
  match String.split result ~on:' ' |> List.filter ~f:(Fn.non is_qualifier) with
  | [ ("bool" | "_Bool") ] -> Codomain.Boolean
  | [ "void" ] -> Unit "void"
  | _ -> Other result
;;

(* A function's type spells its result and then its parameters, as in [int (char * )]. A
   result that points to a function continues with [( * ], and gives no Boolean. *)
let codomain_of aliases declaration =
  match spelling_of declaration with
  | None -> Codomain.Unknown
  | Some spelling ->
    (match String.lsplit2 spelling ~on:'(' with
     | Some (result, parameters) when not (String.is_prefix parameters ~prefix:"*") ->
       codomain_of_result (expand aliases ~visited:String.Set.empty (String.strip result))
     | Some _ | None -> Other spelling)
;;

let declaration_of source node ?definition role =
  let binding =
    let%bind.Option name = text_of node "name" in
    let%bind.Option offset =
      Option.bind (field_of node "loc") ~f:(integer_at ~name:"offset")
    in
    Binding.find source ~offset ?definition ~name role
  in
  Option.value_map binding ~default:(return ()) ~f:Syntax.declare
;;

let has_body node =
  List.exists (children_of node) ~f:(fun child ->
    Option.equal String.equal (text_of child "kind") (Some "CompoundStmt"))
;;

let function_of source aliases node =
  let definition =
    match has_body node with
    | true -> extent_of node
    | false -> None
  in
  declaration_of source node ?definition (Function (lazy (codomain_of aliases node)))
;;

let rec visit_all source aliases nodes =
  match nodes with
  | [] -> return ()
  | node :: rest ->
    let%bind () = visit source aliases node in
    visit_all source (alias_after aliases node) rest

and visit source aliases node =
  match is_spelled_here node with
  | false -> return ()
  | true -> visit_declaration source aliases node

and visit_declaration source aliases node =
  let descend () = visit_all source aliases (children_of node) in
  match text_of node "kind" with
  | Some "FunctionDecl" ->
    let%bind () = function_of source aliases node in
    Syntax.enclose (descend ())
  | Some ("ParmVarDecl" | "VarDecl" | "FieldDecl" | "EnumConstantDecl") ->
    let%bind () = declaration_of source node Variable in
    descend ()
  | Some ("CompoundStmt" | "ForStmt" | "RecordDecl") -> Syntax.enclose (descend ())
  | _ -> descend ()
;;

let arguments_of ~path ~directories ~definitions =
  let options_of flag values = List.concat_map values ~f:(fun value -> [ flag; value ]) in
  List.concat
    [ [ "-x"; "c"; "-fsyntax-only"; "-Xclang"; "-ast-dump=json" ]
    ; [ "-iquote"; Filename.dirname path ]
    ; options_of "-I" directories
    ; options_of "-D" definitions
    ; [ "--"; "-" ]
    ]
;;

let read ~path ~directories ~definitions text =
  let open Or_error.Let_syntax in
  let arguments = arguments_of ~path ~directories ~definitions in
  let%bind output = Process.run ~input:text "clang" arguments in
  let%map tree =
    Or_error.try_with (fun () -> Yojson.Safe.from_string output)
    |> Or_error.tag ~tag:"expected Clang's declaration tree"
  in
  let source = Source.create ~path ~comments:(scan_code text 0 []) text in
  visit source String.Map.empty tree
;;
