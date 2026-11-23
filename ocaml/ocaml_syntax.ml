open! Core
open Ocaml_parsing
open Parsetree

let base = Ast_iterator.default_iterator

type context =
  { source : Source.t
  ; session : Session.t
  ; late_guards : Source.Extent.Set.t
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

let binding_of context ?definition role (name : string Location.loc) =
  let offset = name.loc.loc_start.pos_cnum in
  if is_identifier name.txt
  then Binding.find context.source ~offset ?definition ~name:name.txt role
  else None
;;

let declare context ?definition role name =
  binding_of context ?definition role name
  |> Option.iter ~f:(Syntax.Recorder.declare context.recorder)
;;

(* A function's binding defines its body, and any other binding is declared before it. *)
let introduce context binding ~body =
  match (binding : Binding.t option) with
  | Some ({ role = Function _ | Test; _ } as binding) ->
    Syntax.Recorder.define context.recorder binding body
  | Some binding ->
    Syntax.Recorder.declare context.recorder binding;
    body ()
  | None -> body ()
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

let rec name_of_pattern node =
  match node.ppat_desc with
  | Ppat_var name -> Some name
  | Ppat_constraint (body, _, _) -> name_of_pattern body
  | _ -> None
;;

let define context (iterator : Ast_iterator.iterator) definition =
  let role_of = Typing.role_of_definition context.session definition in
  let descend () = iterator.expr iterator definition.pvb_expr in
  match name_of_pattern definition.pvb_pat with
  | Some name ->
    let extent = Session.extent_of definition.pvb_loc in
    introduce
      context
      (binding_of context ~definition:extent (role_of name) name)
      ~body:descend
  | None ->
    pattern iterator definition.pvb_pat ~binder:(fun name ->
      declare context (role_of name) name);
    descend ()
;;

let specify context _ description =
  let name = description.pval_name in
  declare
    context
    (Typing.role_of_description context.session description.pval_type name)
    name
;;

let label context _ label =
  declare
    context
    (Typing.role_of_label context.session label.pld_type label.pld_name)
    label.pld_name
;;

let classify context (iterator : Ast_iterator.iterator) declaration =
  declare context Type declaration.ptype_name;
  Syntax.Recorder.enclose context.recorder (fun () ->
    base.type_declaration iterator declaration)
;;

let construct context (iterator : Ast_iterator.iterator) declaration =
  declare context Constructor declaration.pcd_name;
  base.constructor_declaration iterator declaration
;;

let extend context (iterator : Ast_iterator.iterator) constructor =
  declare context Constructor constructor.pext_name;
  base.extension_constructor iterator constructor
;;

let furnish context (iterator : Ast_iterator.iterator) field =
  let descend () = base.class_field iterator field in
  let role_of_method = Typing.role_of_method ~extent:field.pcf_loc context.session in
  match field.pcf_desc with
  | Pcf_val (name, _, _) | Pcf_inherit (_, _, Some name) ->
    introduce context (binding_of context Variable name) ~body:descend
  | Pcf_method (name, _, Cfk_virtual annotation) ->
    introduce
      context
      (binding_of context (Typing.role_of_method ~annotation context.session name) name)
      ~body:descend
  | Pcf_method (name, _, Cfk_concrete _) ->
    let definition = Session.extent_of field.pcf_loc in
    introduce
      context
      (binding_of context ~definition (role_of_method name) name)
      ~body:descend
  | _ -> descend ()
;;

let outline context (iterator : Ast_iterator.iterator) field =
  match field.pctf_desc with
  | Pctf_val (name, _, _, _) -> declare context Variable name
  | Pctf_method (name, _, _, annotation) ->
    let extent = field.pctf_loc in
    declare context (Typing.role_of_method ~annotation ~extent context.session name) name
  | _ -> base.class_type_field iterator field
;;

(* An inline test reads as [let%expect_test "description" = body], or with [test] or
   [test_unit] in place of [expect_test]. *)
let test_of context item =
  match item.pstr_desc with
  | Pstr_extension
      ( ( { txt = ("expect_test" | "test" | "test_unit") as kind; _ }
        , PStr [ { pstr_desc = Pstr_value (_, [ definition ]); _ } ] )
      , _ ) ->
    let description =
      match definition.pvb_pat.ppat_desc with
      | Ppat_constant { pconst_desc = Pconst_string (description, _, _); _ } ->
        description
      | _ -> kind
    in
    let offset = item.pstr_loc.loc_start.pos_cnum in
    let extent = Session.extent_of item.pstr_loc in
    Some
      ( Binding.of_test context.source ~offset ~description ~definition:extent
      , definition.pvb_expr )
  | _ -> None
;;

let inspect context (iterator : Ast_iterator.iterator) item =
  match test_of context item with
  | Some (test, body) ->
    introduce context (Some test) ~body:(fun () -> iterator.expr iterator body)
  | None -> base.structure_item iterator item
;;

let decision_of context expression =
  let placement : Decision.Placement.t =
    if Set.mem context.late_guards (Session.extent_of expression.pexp_loc)
    then Late
    else Settled
  in
  let site =
    Source.site_at context.source ~offset:expression.pexp_loc.loc_start.pos_cnum
  in
  let decision_of_form keyword form = { Decision.site; keyword; form; placement } in
  match expression.pexp_desc with
  | Pexp_ifthenelse _ -> Some (decision_of_form "if" If)
  | Pexp_match (_, cases) ->
    Some (decision_of_form "match" (Branching.form_of_cases cases))
  | Pexp_function (_, _, Pfunction_cases (cases, _, _)) ->
    Some (decision_of_form "function" (Branching.form_of_cases cases))
  | _ -> None
;;

(* A chain of [let]s declares its names in one scope, so that each declaration meets its
   siblings, even those declared after it. *)
let rec descend context (iterator : Ast_iterator.iterator) expression =
  match expression.pexp_desc with
  | Pexp_let (_, _, definitions, body) ->
    List.iter definitions ~f:(iterator.value_binding iterator);
    descend context iterator body
  | Pexp_letop { let_ = first; ands = rest; body } ->
    List.iter (first :: rest) ~f:(fun operation ->
      iterator.pat iterator operation.pbop_pat;
      iterator.expr iterator operation.pbop_exp);
    descend context iterator body
  | Pexp_extension (_, PStr [ { pstr_desc = Pstr_eval (body, _); _ } ]) ->
    descend context iterator body
  | _ ->
    Option.iter
      (decision_of context expression)
      ~f:(Syntax.Recorder.decide context.recorder);
    base.expr iterator expression
;;

let unwrap (iterator : Ast_iterator.iterator) = function
  | PStr items -> base.structure iterator items
  | payload -> base.payload iterator payload
;;

let with_bindings context (iterator : Ast_iterator.iterator) : Ast_iterator.iterator =
  { iterator with
    attribute = (fun _ _ -> ())
  ; pat = pattern ~binder:(declare context Variable)
  ; value_binding = define context
  ; value_description = specify context
  ; label_declaration = label context
  ; type_declaration = classify context
  ; constructor_declaration = construct context
  ; extension_constructor = extend context
  ; class_field = furnish context
  ; class_type_field = outline context
  ; structure_item = inspect context
  ; payload = unwrap
  }
;;

let with_scopes context (iterator : Ast_iterator.iterator) : Ast_iterator.iterator =
  let enclose traversal self node =
    Syntax.Recorder.enclose context.recorder (fun () -> traversal self node)
  in
  { iterator with
    structure = enclose base.structure
  ; signature = enclose base.signature
  ; class_structure = enclose base.class_structure
  ; class_signature = enclose base.class_signature
  ; case = enclose base.case
  ; expr = enclose (descend context)
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
  let late_guards = Branching.late_guards_in (Session.parsetree_of session) in
  let program =
    Syntax.record (fun recorder -> traverse { source; session; late_guards; recorder })
  in
  let%map () = Session.verify session in
  program
;;
