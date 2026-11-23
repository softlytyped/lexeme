open! Core
open Ocaml_parsing
open Parsetree

let base = Ast_iterator.default_iterator

type context =
  { source : Source.t
  ; session : Session.t
  ; recorder : Syntax.Recorder.t
  }

(* An operator such as [( @@ )] binds no name. Each byte of a multibyte UTF-8 letter lies
   past ASCII. *)
let is_identifier spelling =
  String.for_all spelling ~f:(fun letter ->
    Char.is_alphanum letter
    || Char.equal letter '_'
    || Char.equal letter '\''
    || Char.( > ) letter '\127')
;;

let bind context ?definition role (name : string Location.loc) =
  let offset = name.loc.loc_start.pos_cnum in
  match is_identifier name.txt with
  | false -> ()
  | true ->
    Binding.find context.source ~offset ?definition ~name:name.txt role
    |> Option.iter ~f:(Syntax.Recorder.declare context.recorder)
;;

let rec pattern ~binder (iterator : Ast_iterator.iterator) node =
  match node.ppat_desc with
  | Ppat_var name -> binder name
  | Ppat_alias (body, name) ->
    pattern ~binder iterator body;
    binder name
  | Ppat_constraint (body, _, _) -> pattern ~binder iterator body
  | _ -> base.pat iterator node
;;

let value_binding context (iterator : Ast_iterator.iterator) definition =
  let role_of = Typing.role_of_definition context.session definition in
  let extent = Session.extent_of definition.pvb_loc in
  pattern iterator definition.pvb_pat ~binder:(fun name ->
    bind context ~definition:extent (role_of name) name);
  iterator.expr iterator definition.pvb_expr
;;

let value_description context _ declaration =
  let name = declaration.pval_name in
  bind context (Typing.role_of_type context.session declaration.pval_type name) name
;;

let label_declaration context _ label =
  bind
    context
    (Typing.role_of_type context.session label.pld_type label.pld_name)
    label.pld_name
;;

let class_field context (iterator : Ast_iterator.iterator) field =
  let session = context.session in
  (match field.pcf_desc with
   | Pcf_val (name, _, _) | Pcf_inherit (_, _, Some name) -> bind context Variable name
   | Pcf_method (name, _, Cfk_virtual annotation) ->
     bind context (Typing.role_of_method ~annotation session name) name
   | Pcf_method (name, _, Cfk_concrete _) ->
     let extent = field.pcf_loc in
     let definition = Session.extent_of extent in
     bind context ~definition (Typing.role_of_method ~extent session name) name
   | _ -> ());
  base.class_field iterator field
;;

let class_type_field context (iterator : Ast_iterator.iterator) field =
  match field.pctf_desc with
  | Pctf_val (name, _, _, _) -> bind context Variable name
  | Pctf_method (name, _, _, annotation) ->
    let extent = field.pctf_loc in
    bind context (Typing.role_of_method ~annotation ~extent context.session name) name
  | _ -> base.class_type_field iterator field
;;

(* A chain of [let]s declares its names in one scope, so that each declaration meets its
   siblings, even those declared after it. *)
let rec descend (iterator : Ast_iterator.iterator) expression =
  match expression.pexp_desc with
  | Pexp_let (_, _, definitions, body) ->
    List.iter definitions ~f:(iterator.value_binding iterator);
    descend iterator body
  | Pexp_letop { let_ = first; ands = rest; body } ->
    List.iter (first :: rest) ~f:(fun operation ->
      iterator.pat iterator operation.pbop_pat;
      iterator.expr iterator operation.pbop_exp);
    descend iterator body
  | Pexp_extension (_, PStr [ { pstr_desc = Pstr_eval (body, _); _ } ]) ->
    descend iterator body
  | _ -> base.expr iterator expression
;;

let payload (iterator : Ast_iterator.iterator) = function
  | PStr items -> base.structure iterator items
  | other -> base.payload iterator other
;;

let with_bindings context (iterator : Ast_iterator.iterator) : Ast_iterator.iterator =
  { iterator with
    attribute = (fun _ _ -> ())
  ; pat = pattern ~binder:(bind context Variable)
  ; value_binding = value_binding context
  ; value_description = value_description context
  ; label_declaration = label_declaration context
  ; class_field = class_field context
  ; class_type_field = class_type_field context
  ; payload
  }
;;

let with_scopes context (iterator : Ast_iterator.iterator) : Ast_iterator.iterator =
  let enclose traversal self node =
    Syntax.Recorder.enclose context.recorder (fun () -> traversal self node)
  in
  { iterator with
    structure = enclose base.structure
  ; signature = enclose base.signature
  ; type_declaration = enclose base.type_declaration
  ; class_structure = enclose base.class_structure
  ; class_signature = enclose base.class_signature
  ; case = enclose base.case
  ; expr = enclose descend
  }
;;

(* The file is the outermost scope, so its items meet the iterator without enclosing. *)
let traverse context =
  let iterator = with_bindings context (with_scopes context base) in
  match Session.parsetree_of context.session with
  | `Interface items -> base.signature iterator items
  | `Implementation items -> base.structure iterator items
;;

let read ~path text =
  let open Or_error.Let_syntax in
  let%bind session = Session.create ~path text in
  let source = Source.create ~path ~comments:(Session.comments_of session) text in
  let program = Syntax.record (fun recorder -> traverse { source; session; recorder }) in
  let%map () = Session.verify session in
  program
;;
