open! Core
open Ocaml_parsing
open Parsetree

let base = Ast_iterator.default_iterator

type context =
  { source : Source.t
  ; session : Session.t
  ; late_guards : Source.Extent.Set.t
  ; nested_booleans : Source.Extent.Set.t
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
  if is_identifier name.txt
  then (
    let offset = name.loc.loc_start.pos_cnum in
    Binding.find context.source ~offset ?definition ~name:name.txt role)
  else None
;;

let declare context ?definition role name =
  binding_of context ?definition role name
  |> Option.iter ~f:(Syntax.Recorder.declare context.recorder)
;;

(* A function's binding defines its body, and any other binding is declared before it. *)
let introduce context binding ~descend =
  match (binding : Binding.t option) with
  | Some ({ role = Function _ | Test; _ } as binding) ->
    Syntax.Recorder.define context.recorder binding descend
  | Some binding ->
    Syntax.Recorder.declare context.recorder binding;
    descend ()
  | None -> descend ()
;;

let rec pattern ~bind (iterator : Ast_iterator.iterator) node =
  match node.ppat_desc with
  | Ppat_var name -> bind name
  | Ppat_alias (body, name) ->
    pattern ~bind iterator body;
    bind name
  | Ppat_constraint (body, _, _) -> pattern ~bind iterator body
  | _ -> base.pat iterator node
;;

let rec name_of_pattern node =
  match node.ppat_desc with
  | Ppat_var name -> Some name
  | Ppat_constraint (body, _, _) -> name_of_pattern body
  | _ -> None
;;

(* A pattern of several names declares each of them before the definition's body. *)
let unpack context (iterator : Ast_iterator.iterator) definition ~role_of =
  pattern iterator definition.pvb_pat ~bind:(fun name ->
    declare context (role_of name) name);
  iterator.expr iterator definition.pvb_expr
;;

(* A value binding of one name introduces it, with its definition's extent. *)
let define context (iterator : Ast_iterator.iterator) definition =
  let role_of = Typing.role_of_definition context.session definition in
  let descend () = iterator.expr iterator definition.pvb_expr in
  let extent = Session.extent_of definition.pvb_loc in
  match name_of_pattern definition.pvb_pat with
  | Some name ->
    introduce context (binding_of context ~definition:extent (role_of name) name) ~descend
  | None -> unpack context iterator definition ~role_of
;;

let specify context _ description =
  let name = description.pval_name in
  let role = Typing.role_of_description context.session description.pval_type name in
  declare context role name
;;

let label context _ label =
  let name = label.pld_name in
  declare context (Typing.role_of_label context.session label.pld_type name) name
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

let concrete_method_of context field name =
  let extent = field.pcf_loc in
  let role = Typing.role_of_method ~extent context.session name in
  binding_of context ~definition:(Session.extent_of extent) role name
;;

let member_of context field =
  match field.pcf_desc with
  | Pcf_val (name, _, _) | Pcf_inherit (_, _, Some name) ->
    binding_of context Variable name
  | Pcf_method (name, _, Cfk_virtual annotation) ->
    binding_of context (Typing.role_of_method ~annotation context.session name) name
  | Pcf_method (name, _, Cfk_concrete _) -> concrete_method_of context field name
  | _ -> None
;;

let furnish context (iterator : Ast_iterator.iterator) field =
  introduce context (member_of context field) ~descend:(fun () ->
    base.class_field iterator field)
;;

let outline context (iterator : Ast_iterator.iterator) field =
  match field.pctf_desc with
  | Pctf_val (name, _, _, _) -> declare context Variable name
  | Pctf_method (name, _, _, annotation) ->
    let extent = field.pctf_loc in
    declare context (Typing.role_of_method ~annotation ~extent context.session name) name
  | _ -> base.class_type_field iterator field
;;

let description_of definition ~kind =
  match definition.pvb_pat.ppat_desc with
  | Ppat_constant { pconst_desc = Pconst_string (description, _, _); _ } -> description
  | _ -> kind
;;

let test_binding_of context item ~description =
  let offset = item.pstr_loc.loc_start.pos_cnum in
  let definition = Session.extent_of item.pstr_loc in
  Binding.of_test context.source ~offset ~description ~definition
;;

(* An inline test reads as [let%expect_test "description" = body], or with [test] or
   [test_unit] in place of [expect_test]. *)
let test_of context item =
  match item.pstr_desc with
  | Pstr_extension
      ( ( { txt = ("expect_test" | "test" | "test_unit") as kind; _ }
        , PStr [ { pstr_desc = Pstr_value (_, [ definition ]); _ } ] )
      , _ ) ->
    Some
      ( test_binding_of context item ~description:(description_of definition ~kind)
      , definition.pvb_expr )
  | _ -> None
;;

let inspect context (iterator : Ast_iterator.iterator) item =
  match test_of context item with
  | Some (test, body) ->
    introduce context (Some test) ~descend:(fun () -> iterator.expr iterator body)
  | None -> base.structure_item iterator item
;;

let placement_of context extent : Decision.Placement.t =
  if Set.mem context.late_guards extent then Late else Settled
;;

let nesting_of context extent : Decision.Nesting.t =
  if Set.mem context.nested_booleans extent then Nested else Outermost
;;

let branching_of expression =
  match expression.pexp_desc with
  | Pexp_ifthenelse _ -> Some ("if", Decision.Form.Conditional)
  | Pexp_match (_, cases) -> Some ("match", Branching.form_of_cases cases)
  | Pexp_function (_, _, Pfunction_cases (cases, _, _)) ->
    Some ("function", Branching.form_of_cases cases)
  | _ -> None
;;

let decision_of context expression =
  let%map.Option keyword, form = branching_of expression in
  let extent = Session.extent_of expression.pexp_loc in
  { Decision.site = Source.site_at context.source ~offset:extent.start
  ; keyword
  ; form
  ; placement = placement_of context extent
  ; nesting = nesting_of context extent
  }
;;

(* A link of a chain of [let]s declares its names and gives the rest of the chain. *)
let after_definitions (iterator : Ast_iterator.iterator) definitions ~rest =
  List.iter definitions ~f:(iterator.value_binding iterator);
  rest
;;

let after_operations (iterator : Ast_iterator.iterator) operation =
  List.iter (operation.let_ :: operation.ands) ~f:(fun binding_operator ->
    iterator.pat iterator binding_operator.pbop_pat;
    iterator.expr iterator binding_operator.pbop_exp);
  operation.body
;;

let visit context (iterator : Ast_iterator.iterator) expression =
  Option.iter
    (decision_of context expression)
    ~f:(Syntax.Recorder.decide context.recorder);
  base.expr iterator expression
;;

(* A chain of [let]s declares its names in one scope, so that each declaration meets its
   siblings, even those declared after it. *)
let rec descend context (iterator : Ast_iterator.iterator) expression =
  match expression.pexp_desc with
  | Pexp_let (_, _, definitions, rest) ->
    descend context iterator (after_definitions iterator definitions ~rest)
  | Pexp_letop operation -> descend context iterator (after_operations iterator operation)
  | Pexp_extension (_, PStr [ { pstr_desc = Pstr_eval (body, _); _ } ]) ->
    descend context iterator body
  | _ -> visit context iterator expression
;;

let unwrap (iterator : Ast_iterator.iterator) = function
  | PStr items -> base.structure iterator items
  | payload -> base.payload iterator payload
;;

let with_values context (iterator : Ast_iterator.iterator) : Ast_iterator.iterator =
  let bind name = declare context (Typing.role_of_pattern context.session name) name in
  { iterator with
    attribute = (fun _ _ -> ())
  ; pat = pattern ~bind
  ; value_binding = define context
  ; value_description = specify context
  ; structure_item = inspect context
  ; payload = unwrap
  }
;;

let with_types context (iterator : Ast_iterator.iterator) : Ast_iterator.iterator =
  { iterator with
    label_declaration = label context
  ; type_declaration = classify context
  ; constructor_declaration = construct context
  ; extension_constructor = extend context
  ; class_field = furnish context
  ; class_type_field = outline context
  }
;;

let nest context traverse self node =
  Syntax.Recorder.enclose context.recorder (fun () -> traverse self node)
;;

let with_scopes context (iterator : Ast_iterator.iterator) : Ast_iterator.iterator =
  let enclose traverse = nest context traverse in
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
  let iterator = with_values context (with_types context (with_scopes context base)) in
  match Session.syntax_tree_of context.session with
  | `Interface items -> base.signature iterator items
  | `Implementation items -> base.structure iterator items
;;

let program_of source session =
  let syntax_tree = Session.syntax_tree_of session in
  let late_guards = Branching.late_guards_in syntax_tree in
  let nested_booleans = Branching.nested_booleans_in syntax_tree in
  Syntax.record (fun recorder ->
    traverse { source; session; late_guards; nested_booleans; recorder })
;;

let read ~path text =
  let open Or_error.Let_syntax in
  let%bind session = Session.create ~path text in
  let source = Source.create ~path ~comments:(Session.comments_of session) text in
  let program = program_of source session in
  let%map () = Session.verify session in
  program
;;
