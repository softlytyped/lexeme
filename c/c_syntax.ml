open! Core
open Syntax.Let_syntax
module Codomain = Binding.Codomain

(* We follow C's lexical states only as far as comments need them, since a string or a
   character constant can hold a comment's delimiters. *)
let line_opening = "//"
let block_opening = "/*"
let block_closing = "*/"

let character_at text offset =
  if offset < String.length text then Some text.[offset] else None
;;

let comment_of text ~start ~stop ~closing =
  let body_start = start + String.length block_opening in
  { Source.Comment.extent = { start; stop }
  ; body = String.slice text body_start (stop - String.length closing)
  }
;;

let rec comments_in_code text offset comments =
  let does_open opening = String.is_substring_at text ~pos:offset ~substring:opening in
  match character_at text offset with
  | None -> List.rev comments
  | Some '/' when does_open line_opening ->
    comments_in_line text ~start:offset (offset + String.length line_opening) comments
  | Some '/' when does_open block_opening ->
    comments_in_block text ~start:offset (offset + String.length block_opening) comments
  | Some (('"' | '\'') as quote) -> comments_in_quote text ~quote (offset + 1) comments
  | Some _ -> comments_in_code text (offset + 1) comments

and comments_in_quote text ~quote offset comments =
  match character_at text offset with
  | None -> List.rev comments
  | Some '\\' -> comments_in_escape text ~quote (offset + 1) comments
  | Some letter when Char.equal letter quote || Char.equal letter '\n' ->
    comments_in_code text (offset + 1) comments
  | Some _ -> comments_in_quote text ~quote (offset + 1) comments

and comments_in_escape text ~quote offset comments =
  comments_in_quote text ~quote (offset + 1) comments

(* A backslash at the end of a line continues a line comment onto the next. *)
and comments_in_line text ~start offset comments =
  let close () = comment_of text ~start ~stop:offset ~closing:"" :: comments in
  match character_at text offset with
  | None -> List.rev (close ())
  | Some '\n' when not (Char.equal text.[offset - 1] '\\') ->
    comments_in_code text offset (close ())
  | Some _ -> comments_in_line text ~start (offset + 1) comments

and comments_in_block text ~start offset comments =
  match character_at text offset with
  | None -> List.rev (comment_of text ~start ~stop:offset ~closing:"" :: comments)
  | Some '*' when String.is_substring_at text ~pos:offset ~substring:block_closing ->
    let stop = offset + String.length block_closing in
    comments_in_code
      text
      stop
      (comment_of text ~start ~stop ~closing:block_closing :: comments)
  | Some _ -> comments_in_block text ~start (offset + 1) comments
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

let rec expand aliases ~expansions spelling =
  match Map.find aliases spelling with
  | Some target when not (Set.mem expansions spelling) ->
    expand aliases ~expansions:(Set.add expansions spelling) target
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
  | _ -> Other_type result
;;

(* A function's type spells its result and then its parameters, as in [int (char * )]. A
   result that points to a function continues with [( * ], and gives no Boolean. *)
let codomain_of aliases declaration =
  match spelling_of declaration with
  | None -> Codomain.Unknown
  | Some spelling ->
    (match String.lsplit2 spelling ~on:'(' with
     | Some (result, parameters) when not (String.is_prefix parameters ~prefix:"*") ->
       codomain_of_result
         (expand aliases ~expansions:String.Set.empty (String.strip result))
     | Some _ | None -> Other_type spelling)
;;

let binding_of source node ?definition role =
  let%bind.Option name = text_of node "name" in
  let%bind.Option offset =
    Option.bind (field_of node "loc") ~f:(integer_at ~name:"offset")
  in
  Binding.find source ~offset ?definition ~name role
;;

let declaration_of source node role =
  Option.value_map (binding_of source node role) ~default:(return ()) ~f:Syntax.declare
;;

let has_body node =
  List.exists (children_of node) ~f:(fun child ->
    Option.equal String.equal (text_of child "kind") (Some "CompoundStmt"))
;;

(* A function's definition defines its body, and a prototype declares the function before
   its parameters' scope. *)
let function_of source aliases node ~body =
  let role = Binding.Role.Function (lazy (codomain_of aliases node)) in
  let definition = if has_body node then extent_of node else None in
  match binding_of source node ?definition role, definition with
  | Some binding, Some _ -> Syntax.define binding body
  | Some binding, None -> Syntax.declare binding >>= fun () -> Syntax.enclose body
  | None, _ -> Syntax.enclose body
;;

let field_of_node aliases node =
  let expand spelling =
    expand aliases ~expansions:String.Set.empty (String.strip spelling)
  in
  let codomain_of spelling = codomain_of_result (expand spelling) in
  Binding.Role.Field
    (lazy (Option.value_map (spelling_of node) ~default:Codomain.Unknown ~f:codomain_of))
;;

(* Enumeration constants share the enclosing scope, and a structure's fields do not. *)
let is_block = function
  | "CompoundStmt" | "ForStmt" | "RecordDecl" -> true
  | _ -> false
;;

let role_of aliases node kind =
  match kind with
  | "ParmVarDecl" | "VarDecl" -> Some Binding.Role.Variable
  | "FieldDecl" -> Some (field_of_node aliases node)
  | "EnumConstantDecl" -> Some Constructor
  | "TypedefDecl" | "EnumDecl" | "RecordDecl" -> Some Type
  | _ -> None
;;

let rec visit_all source aliases nodes =
  match nodes with
  | [] -> return ()
  | node :: rest ->
    let%bind () = visit source aliases node in
    visit_all source (alias_after aliases node) rest

and visit source aliases node =
  if is_spelled_here node then dispatch source aliases node else return ()

(* A declaration declares its name before its body, and a block encloses its body. *)
and dispatch source aliases node =
  let kind = Option.value (text_of node "kind") ~default:"" in
  let descend () = visit_all source aliases (children_of node) in
  let body = if is_block kind then Syntax.enclose (descend ()) else descend () in
  match kind, role_of aliases node kind with
  | "FunctionDecl", _ -> function_of source aliases node ~body
  | _, Some role -> declaration_of source node role >>= fun () -> body
  | _, None -> body
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
  let source = Source.create ~path ~comments:(comments_in_code text 0 []) text in
  visit source String.Map.empty tree
;;
