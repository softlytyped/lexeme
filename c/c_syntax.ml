open! Core
open Binding_syntax

let field node name =
  match node with
  | `Assoc fields -> List.Assoc.find fields name ~equal:String.equal
  | _ -> None
;;

let text_of node name =
  match field node name with
  | Some (`String text) -> Some text
  | _ -> None
;;

let children_of node =
  match field node "inner" with
  | Some (`List children) -> children
  | _ -> []
;;

let spelling_of node =
  let%bind.Option description = field node "type" in
  match text_of description "desugaredQualType" with
  | Some _ as spelling -> spelling
  | None -> text_of description "qualType"
;;

let result_of aliases declaration =
  let rec expand visited_types spelling =
    match Map.find aliases spelling with
    | Some target when not (Set.mem visited_types spelling) ->
      expand (Set.add visited_types spelling) target
    | _ -> spelling
  in
  match Option.bind (spelling_of declaration) ~f:(String.lsplit2 ~on:'(') with
  | None -> Unknown
  | Some (result, arguments) ->
    let result = expand String.Set.empty (String.strip result) in
    let words =
      String.split result ~on:' '
      |> List.filter ~f:(function
        | "const" | "volatile" -> false
        | _ -> true)
    in
    (match words with
     | [ ("bool" | "_Bool") ] when not (String.is_prefix arguments ~prefix:"*") -> Boolean
     | _ -> Other result)
;;

let fold source ~directories ~definitions algebra =
  let open Or_error.Let_syntax in
  let path = Source.path_of source in
  let options_of flag values = List.concat_map values ~f:(fun value -> [ flag; value ]) in
  let arguments =
    [ "-x"; "c"; "-fsyntax-only"; "-Xclang"; "-ast-dump=json" ]
    @ [ "-iquote"; Filename.dirname path ]
    @ options_of "-I" directories
    @ options_of "-D" definitions
    @ [ "--"; "-" ]
  in
  let%bind output = Process.read ~input:(Source.text_of source) "clang" arguments in
  let%bind tree =
    Or_error.try_with (fun () -> Yojson.Safe.from_string output)
    |> Or_error.tag ~tag:"expected Clang's declaration tree"
  in
  let bind node location role =
    let open Option.Let_syntax in
    let%bind name = text_of node "name" in
    match field location "offset" with
    | Some (`Int offset) ->
      let%map binding = binding_at source ~offset ~name role in
      algebra.bind binding
    | _ -> None
  in
  let rec descend aliases nodes =
    let visit aliases node =
      let aliases =
        match text_of node "kind", text_of node "name", spelling_of node with
        | Some "TypedefDecl", Some name, Some spelling ->
          Map.set aliases ~key:name ~data:spelling
        | _ -> aliases
      in
      aliases, visit aliases node
    in
    List.folding_map nodes ~init:aliases ~f:visit |> List.concat
  and visit aliases node =
    let location = Option.value (field node "loc") ~default:`Null in
    (* We recognize headers by their include site even where Clang elides the filename.
       Macro expansions carry separate spelling and expansion locations. *)
    match field location "includedFrom", field location "expansionLoc" with
    | Some _, _ | _, Some _ -> []
    | None, None ->
      let traverse () = descend aliases (children_of node) in
      (match text_of node "kind" with
       | Some "FunctionDecl" ->
         Option.to_list (bind node location (Function (lazy (result_of aliases node))))
         @ [ algebra.enclose (traverse ()) ]
       | Some ("ParmVarDecl" | "VarDecl" | "FieldDecl" | "EnumConstantDecl") ->
         Option.to_list (bind node location Variable) @ traverse ()
       | Some ("CompoundStmt" | "ForStmt" | "RecordDecl") ->
         [ algebra.enclose (traverse ()) ]
       | _ -> traverse ())
  in
  Ok (algebra.enclose (visit String.Map.empty tree))
;;
