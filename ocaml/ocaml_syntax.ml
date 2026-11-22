open! Core
open Ocaml_parsing
open Parsetree
open Binding_syntax

let base = Ast_iterator.default_iterator

let is_identifier name =
  String.for_all name ~f:(fun letter ->
    Char.is_alphanum letter
    || Char.equal letter '_'
    || Char.equal letter '\''
    || Char.to_int letter > Char.to_int '\127')
;;

let rec descend (syntax : Ast_iterator.iterator) expression =
  match expression.pexp_desc with
  | Pexp_let (_, _, definitions, body) ->
    List.iter definitions ~f:(syntax.value_binding syntax);
    descend syntax body
  | Pexp_letop { let_ = first; ands = rest; body } ->
    List.iter (first :: rest) ~f:(fun operation ->
      syntax.pat syntax operation.pbop_pat;
      syntax.expr syntax operation.pbop_exp);
    descend syntax body
  | Pexp_extension (_, PStr [ { pstr_desc = Pstr_eval (body, _); _ } ]) ->
    descend syntax body
  | _ -> base.expr syntax expression
;;

let syntax_of source types ~declare:writer ~scope =
  let bind role (name : string Location.loc) =
    match is_identifier name.txt with
    | false -> ()
    | true ->
      let offset = name.loc.loc_start.pos_cnum in
      Option.iter (binding_at source ~offset ~name:name.txt role) ~f:writer
  in
  let enclose traversal _ node = scope (fun syntax -> traversal syntax node) in
  let rec pattern (syntax : Ast_iterator.iterator) classification node =
    match node.ppat_desc with
    | Ppat_var name -> bind (classification name) name
    | Ppat_alias (body, name) ->
      pattern syntax classification body;
      bind (classification name) name
    | Ppat_constraint (body, _, _) -> pattern syntax classification body
    | _ -> base.pat syntax node
  in
  { base with
    attribute = (fun _ _ -> ())
  ; pat = (fun syntax -> pattern syntax (fun _ -> Variable))
  ; value_binding =
      (fun syntax definition ->
        pattern syntax (Typing.role_of_definition types definition) definition.pvb_pat;
        syntax.expr syntax definition.pvb_expr)
  ; value_description =
      (fun _ declaration ->
        let name = declaration.pval_name in
        bind (Typing.role_of_type types declaration.pval_type name) name)
  ; label_declaration =
      (fun _ label ->
        let name = label.pld_name in
        bind (Typing.role_of_type types label.pld_type name) name)
  ; class_field =
      (fun syntax field ->
        (match field.pcf_desc with
         | Pcf_val (name, _, _) | Pcf_inherit (_, _, Some name) -> bind Variable name
         | Pcf_method (name, _, Cfk_virtual annotation) ->
           bind (Typing.method_at ~annotation types name) name
         | Pcf_method (name, _, Cfk_concrete _) ->
           bind (Typing.method_at ~extent:field.pcf_loc types name) name
         | _ -> ());
        base.class_field syntax field)
  ; class_type_field =
      (fun syntax field ->
        match field.pctf_desc with
        | Pctf_val (name, _, _, _) -> bind Variable name
        | Pctf_method (name, _, _, annotation) ->
          bind (Typing.method_at ~annotation ~extent:field.pctf_loc types name) name
        | _ -> base.class_type_field syntax field)
  ; structure = enclose base.structure
  ; signature = enclose base.signature
  ; type_declaration = enclose base.type_declaration
  ; class_structure = enclose base.class_structure
  ; class_signature = enclose base.class_signature
  ; case = enclose base.case
  ; expr = enclose descend
  ; payload =
      (fun syntax -> function
        | PStr items -> base.structure syntax items
        | payload -> base.payload syntax payload)
  }
;;

let fold source algebra =
  let open Or_error.Let_syntax in
  let%bind tree, types = Inferred.read source in
  let rec children_of traversal =
    let children = Queue.create () in
    let declare binding = Queue.enqueue children (algebra.bind binding) in
    let enclose traversal =
      match children_of traversal with
      | [] -> ()
      | children_in_scope -> Queue.enqueue children (algebra.enclose children_in_scope)
    in
    traversal (syntax_of source types ~declare ~scope:enclose);
    Queue.to_list children
  in
  let file =
    children_of (fun syntax ->
      match tree with
      | `Interface items -> base.signature syntax items
      | `Implementation items -> base.structure syntax items)
  in
  let%map () = Inferred.verify types in
  algebra.enclose file
;;
