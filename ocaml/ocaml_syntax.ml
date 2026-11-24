open! Core
open Ocaml_parsing
open Parsetree

let base = Ast_iterator.default_iterator

type context =
  { source : Source.t
  ; session : Session.t
  ; late_guards : Source.Extent.Set.t
  ; nested_booleans : Source.Extent.Set.t
  ; idioms : (Idiom.Form.t * string) list Source.Extent.Map.t
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

let site_at context location =
  Source.site_at
    context.source
    ~offset:(Source.Extent.start_of (Session.extent_of location))
;;

let binding_of context ?definition role (name : string Location.loc) =
  if is_identifier name.txt
  then (
    let offset = Source.Extent.start_of (Session.extent_of name.loc) in
    Binding.find context.source ~offset ?definition ~name:name.txt role)
  else None
;;

let declare context ?definition role name =
  Option.iter
    (binding_of context ?definition role name)
    ~f:(Syntax.Recorder.declare context.recorder)
;;

let has_body binding =
  match Binding.role_of binding with
  | Function _ | Test -> true
  | Variable | Field _ | Type | Constructor -> false
;;

(* A function's binding defines its body, and any other binding is declared before it. *)
let introduce context binding ~descend =
  match binding with
  | Some binding when has_body binding ->
    Syntax.Recorder.define context.recorder binding descend
  | Some binding ->
    Syntax.Recorder.declare context.recorder binding;
    descend ()
  | None -> descend ()
;;

let fields_of_pattern node =
  match node.ppat_desc with
  | Ppat_record (fields, _) -> List.map fields ~f:fst
  | _ -> []
;;

(* A pattern binds its names and reaches the fields of its records, under a constraint or
   an alias too. *)
let rec pattern ~bind ~reach (iterator : Ast_iterator.iterator) node =
  List.iter (fields_of_pattern node) ~f:reach;
  match node.ppat_desc with
  | Ppat_var name -> bind name
  | Ppat_alias (body, name) ->
    pattern ~bind ~reach iterator body;
    bind name
  | Ppat_constraint (body, _, _) -> pattern ~bind ~reach iterator body
  | _ -> base.pat iterator node
;;

(* A field of a record that another compilation unit defines is an access. *)
let access_of context (field : Longident.t Location.loc) =
  let extent = Session.extent_of field.loc in
  let%map.Option owner = Session.owner_at context.session extent in
  Access.create
    ~site:(Source.site_at context.source ~offset:(Source.Extent.start_of extent))
    ~field:(Longident.last field.txt)
    ~owner
;;

let reach context field =
  Option.iter (access_of context field) ~f:(Syntax.Recorder.access context.recorder)
;;

let rec name_of_pattern node =
  match node.ppat_desc with
  | Ppat_var name -> Some name
  | Ppat_constraint (body, _, _) -> name_of_pattern body
  | _ -> None
;;

(* A pattern of several names declares each of them before the definition's body. *)
let unpack context (iterator : Ast_iterator.iterator) definition ~role_of =
  pattern iterator definition.pvb_pat ~reach:(reach context) ~bind:(fun name ->
    declare context (role_of name) name);
  iterator.expr iterator definition.pvb_expr
;;

let is_boolean_codomain : Binding.Codomain.t -> bool = function
  | Boolean -> true
  | Optional_boolean | Fallible_boolean | Unit _ | Other_type _ | Unknown -> false
;;

let is_boolean_value : _ Session.Shape.t option -> bool = function
  | Some (Value codomain) -> is_boolean_codomain (force codomain)
  | Some (Function _) | None -> false
;;

let is_boolean context (name : string Location.loc) =
  is_boolean_value (Session.shape_at context.session (Session.extent_of name.loc))
;;

(* A named function's parameter of type bool is an idiom, which a variant could replace. *)
let screen context definition =
  List.filter_map (Idioms.parameters_of definition.pvb_expr) ~f:name_of_pattern
  |> List.filter ~f:(is_boolean context)
  |> List.iter ~f:(fun (name : string Location.loc) ->
    let site = site_at context name.loc in
    Syntax.Recorder.express
      context.recorder
      (Idiom.create ~site ~subject:name.txt Boolean_parameter))
;;

(* A value binding of one name introduces it, with its definition's extent. *)
let define context (iterator : Ast_iterator.iterator) definition =
  let role_of = Typing.role_of_definition context.session definition in
  let descend () = iterator.expr iterator definition.pvb_expr in
  let extent = Session.extent_of definition.pvb_loc in
  match name_of_pattern definition.pvb_pat with
  | Some name ->
    screen context definition;
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
  let definition = Session.extent_of item.pstr_loc in
  let offset = Source.Extent.start_of definition in
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
  if Set.mem context.late_guards extent then Late_position else Proper_position
;;

let nesting_of context extent : Decision.Nesting.t =
  if Set.mem context.nested_booleans extent then Nested_position else Outer_position
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
  let extent = Session.extent_of expression.pexp_loc in
  let%map.Option keyword, form = branching_of expression in
  Decision.create
    ~site:(Source.site_at context.source ~offset:(Source.Extent.start_of extent))
    ~keyword
    ~form
    ~placement:(placement_of context extent)
    ~nesting:(nesting_of context extent)
;;

(* An extension such as [bind] or [bind.Option] names the operation of [let%bind]. *)
let operation_of_extension name : Notation.Operation.t option =
  match String.split name ~on:'.' with
  | "bind" :: _ -> Some Bind
  | "map" :: _ -> Some Map
  | _ -> None
;;

let operation_of_operator operator : Notation.Operation.t option =
  match operator with
  | ">>=" | "let*" -> Some Bind
  | ">>|" | "let+" -> Some Map
  | _ -> None
;;

let sequence context location operation ~form =
  Option.iter operation ~f:(fun operation ->
    Syntax.Recorder.sequence
      context.recorder
      (Notation.create ~site:(site_at context location) ~operation ~form))
;;

let rec definition_of expression : Chain.Link.Definition.t =
  match expression.pexp_desc with
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "|>"; _ }; _ }, _) -> Pipeline
  | Pexp_constraint (body, _, _) -> definition_of body
  | _ -> Other_expression
;;

let names_bound_by node =
  let names = Queue.create () in
  let bind (name : string Location.loc) = Queue.enqueue names name.txt in
  let iterator = { base with pat = pattern ~bind ~reach:ignore } in
  iterator.pat iterator node;
  Queue.to_list names
;;

(* An identifier of the expression's own module may name a local definition. *)
let local_name_of node =
  match node.pexp_desc with
  | Pexp_ident { txt = Lident name; _ } -> Some name
  | _ -> None
;;

let names_read_by expression =
  let names = Queue.create () in
  let gather (iterator : Ast_iterator.iterator) node =
    Option.iter (local_name_of node) ~f:(Queue.enqueue names);
    base.expr iterator node
  in
  let iterator = { base with expr = gather } in
  iterator.expr iterator expression;
  Queue.to_list names
;;

let link_of context ~keyword ~form pattern definition =
  Chain.Link.create
    ~site:(site_at context pattern.ppat_loc)
    ~keyword
    ~form
    ~definition:(definition_of definition)
    ~names:(names_bound_by pattern)
    ~reads:(names_read_by definition)
;;

(* The links after the first in one [let] open with [and]. *)
let keyword_at place ~keyword =
  match place with
  | 0 -> keyword
  | _ -> "and"
;;

(* A link of a chain of [let]s declares its names. *)
let links_of_definitions context iterator definitions ~keyword ~form =
  List.mapi definitions ~f:(fun place definition ->
    iterator.Ast_iterator.value_binding iterator definition;
    let keyword = keyword_at place ~keyword in
    link_of context ~keyword ~form definition.pvb_pat definition.pvb_expr)
;;

let links_of_plain_let context iterator definitions =
  links_of_definitions context iterator definitions ~keyword:"let" ~form:Plain_binding
;;

let links_of_extension context iterator (extension : string Location.loc) definitions =
  sequence context extension.loc (operation_of_extension extension.txt) ~form:Let_syntax;
  let keyword = "let%" ^ extension.txt in
  links_of_definitions context iterator definitions ~keyword ~form:Monad_binding
;;

let link_of_operator context (iterator : Ast_iterator.iterator) binding_operator =
  let pattern = binding_operator.pbop_pat
  and definition = binding_operator.pbop_exp in
  iterator.pat iterator pattern;
  iterator.expr iterator definition;
  let keyword = binding_operator.pbop_op.txt in
  link_of context ~keyword ~form:Monad_binding pattern definition
;;

let links_of_operators context iterator operation =
  let operator = operation.let_.pbop_op in
  sequence context operator.loc (operation_of_operator operator.txt) ~form:Let_syntax;
  List.map (operation.let_ :: operation.ands) ~f:(link_of_operator context iterator)
;;

let close context links =
  match links with
  | [] -> ()
  | _ :: _ -> Syntax.Recorder.chain context.recorder (Chain.create links)
;;

(* A field that an expression reads or updates is an access. A record built in full names
   each of its fields, as a constructor names its arguments, so it reads none of them. *)
let observe context expression =
  match expression.pexp_desc with
  | Pexp_field (_, field) | Pexp_setfield (_, field, _) -> reach context field
  | Pexp_record (fields, Some _) ->
    List.iter fields ~f:(fun (field, _) -> reach context field)
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident name; loc = location }; _ }, _) ->
    sequence context location (operation_of_operator name) ~form:Infix_operator
  | _ -> ()
;;

let express context expression =
  let site = site_at context expression.pexp_loc in
  let forms = Map.find_multi context.idioms (Session.extent_of expression.pexp_loc) in
  List.iter forms ~f:(fun (form, subject) ->
    Syntax.Recorder.express context.recorder (Idiom.create ~site ~subject form))
;;

let visit context (iterator : Ast_iterator.iterator) expression =
  Option.iter
    (decision_of context expression)
    ~f:(Syntax.Recorder.decide context.recorder);
  observe context expression;
  express context expression;
  base.expr iterator expression
;;

(* A step along a chain of [let]s, where [let%bind] is the extension [bind] around a
   [let], and the chain ends at its body. *)
module Step = struct
  type t =
    | Plain_let of value_binding list * expression
    | Extended_let of string Location.loc * value_binding list * expression
    | Operator_let of letop
    | Extension of expression
    | Body

  let of_extension extension body =
    match body.pexp_desc with
    | Pexp_let (_, _, definitions, rest) -> Extended_let (extension, definitions, rest)
    | _ -> Extension body
  ;;

  let of_expression expression =
    match expression.pexp_desc with
    | Pexp_let (_, _, definitions, rest) -> Plain_let (definitions, rest)
    | Pexp_letop operation -> Operator_let operation
    | Pexp_extension (extension, PStr [ { pstr_desc = Pstr_eval (body, _); _ } ]) ->
      of_extension extension body
    | _ -> Body
  ;;
end

(* Each step declares the names of its links and gives the rest of the chain. *)
let links_at context iterator : Step.t -> _ = function
  | Plain_let (definitions, rest) ->
    Some (links_of_plain_let context iterator definitions, rest)
  | Extended_let (extension, definitions, rest) ->
    Some (links_of_extension context iterator extension definitions, rest)
  | Operator_let operation ->
    Some (links_of_operators context iterator operation, operation.body)
  | Extension body -> Some ([], body)
  | Body -> None
;;

(* A chain of [let]s declares its names in one scope, so that each declaration meets its
   siblings, even those declared after it. We gather its links on the way to its body. *)
let rec follow context iterator expression ~links =
  match links_at context iterator (Step.of_expression expression) with
  | Some (more_links, rest) -> follow context iterator rest ~links:(links @ more_links)
  | None ->
    close context links;
    visit context iterator expression
;;

let descend context iterator expression = follow context iterator expression ~links:[]

let unwrap (iterator : Ast_iterator.iterator) = function
  | PStr items -> base.structure iterator items
  | payload -> base.payload iterator payload
;;

let with_values context (iterator : Ast_iterator.iterator) : Ast_iterator.iterator =
  let bind name = declare context (Typing.role_of_pattern context.session name) name in
  { iterator with
    attribute = (fun _ _ -> ())
  ; pat = pattern ~bind ~reach:(reach context)
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
  let idioms = Idioms.forms_in syntax_tree in
  Syntax.record (fun recorder ->
    traverse { source; session; late_guards; nested_booleans; idioms; recorder })
;;

let read ~path text =
  let open Or_error.Let_syntax in
  let%bind session = Session.create ~path text in
  let source = Source.create ~path ~comments:(Session.comments_of session) text in
  let program = program_of source session in
  let%map () = Session.verify session in
  program
;;
