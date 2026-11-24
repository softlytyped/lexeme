open! Core
open Ocaml_parsing
open Parsetree

let base = Ast_iterator.default_iterator

let iterate (iterator : Ast_iterator.iterator) = function
  | `Interface items -> iterator.signature iterator items
  | `Implementation items -> iterator.structure iterator items
;;

let extent_of expression = Session.extent_of expression.pexp_loc

(* Returns what [find] finds at the expressions of a source, in order. *)
let collect syntax_tree ~find =
  let found = Queue.create () in
  let visit self expression =
    Option.iter (find expression) ~f:(Queue.enqueue found);
    base.expr self expression
  in
  iterate { base with expr = visit } syntax_tree;
  Queue.to_list found
;;

let rec definition_of expression =
  match expression.pexp_desc with
  | Pexp_constraint (body, _, _) | Pexp_newtype (_, _, body) -> definition_of body
  | _ -> expression
;;

let parameters_of expression =
  match (definition_of expression).pexp_desc with
  | Pexp_function (parameters, _, _) ->
    List.filter_map parameters ~f:(fun parameter ->
      match parameter.pparam_desc with
      | Pparam_val (_, _, pattern) -> Some pattern
      | Pparam_newtype _ -> None)
  | _ -> []
;;

let is_boolean_literal expression =
  match expression.pexp_desc with
  | Pexp_construct ({ txt = Lident ("true" | "false"); _ }, None) -> true
  | _ -> false
;;

(* The body applies a function to the parameter alone, and no other part names it. *)
let is_application_to_parameter parameter (operator, arguments) =
  match List.rev arguments with
  | (Nolabel, { pexp_desc = Pexp_ident { txt = Lident last; _ }; _ }) :: other_arguments
    ->
    String.equal last parameter
    && List.for_all (operator :: List.map other_arguments ~f:snd) ~f:(fun operand ->
      not (Set.mem (Branching.identifiers_in operand) parameter))
  | _ -> false
;;

let is_eta_expansion parameters body =
  match parameters, body with
  | ( [ { pparam_desc = Pparam_val (Nolabel, None, { ppat_desc = Ppat_var name; _ }); _ } ]
    , Pfunction_body { pexp_desc = Pexp_apply (operator, arguments); _ } ) ->
    is_application_to_parameter name.txt (operator, arguments)
  | _ -> false
;;

let is_one_case = function
  | Pfunction_cases ([ { pc_guard = None; _ } ], _, _) -> true
  | Pfunction_cases _ | Pfunction_body _ -> false
;;

let is_zero expression =
  match expression.pexp_desc with
  | Pexp_constant { pconst_desc = Pconst_integer ("0", None); _ } -> true
  | _ -> false
;;

let length_functions =
  [ [ "List"; "length" ]; [ "String"; "length" ]; [ "Array"; "length" ] ]
;;

let is_length expression =
  match expression.pexp_desc with
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = path; _ }; _ }, [ _ ]) ->
    List.mem length_functions (Longident.flatten path) ~equal:(List.equal String.equal)
  | _ -> false
;;

let comparisons = String.Set.of_list [ "="; "<>"; "<"; ">"; "<="; ">="; "=="; "!=" ]

(* A length compared with zero, on either side of the comparison. *)
let is_length_comparison operator operands =
  match operands with
  | [ (Nolabel, left); (Nolabel, right) ] ->
    Set.mem comparisons operator
    && ((is_length left && is_zero right) || (is_zero left && is_length right))
  | _ -> false
;;

let application_form_of operator operands =
  match operator, operands with
  | "ignore", [ (Nolabel, { pexp_desc = Pexp_constraint _; _ }) ] -> None
  | "ignore", [ (Nolabel, _) ] -> Some (Idiom.Form.Discard_without_type, "ignore")
  | _ ->
    Option.some_if
      (is_length_comparison operator operands)
      (Idiom.Form.Length_comparison, operator)
;;

(* The idiom that an expression is by its own shape. *)
let form_of expression =
  match expression.pexp_desc with
  | Pexp_ifthenelse (_, consequent, Some alternative)
    when is_boolean_literal consequent && is_boolean_literal alternative ->
    Some (Idiom.Form.Boolean_branches, "if")
  | Pexp_function (_, _, body) when is_one_case body ->
    Some (Idiom.Form.One_case_function, "function")
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident operator; _ }; _ }, operands) ->
    application_form_of operator operands
  | _ -> None
;;

let local_forms_in syntax_tree =
  collect syntax_tree ~find:(fun expression ->
    Option.map (form_of expression) ~f:(fun form -> extent_of expression, form))
;;

let operand_of_pipeline expression =
  match expression.pexp_desc with
  | Pexp_apply
      ({ pexp_desc = Pexp_ident { txt = Lident "|>"; _ }; _ }, [ (Nolabel, operand); _ ])
    -> Some operand
  | _ -> None
;;

let is_pipeline expression = Option.is_some (operand_of_pipeline expression)

let pipelines_in syntax_tree =
  collect syntax_tree ~find:(fun expression ->
    Option.map (operand_of_pipeline expression) ~f:(fun operand -> expression, operand))
;;

(* A pipeline whose operand is a pipeline has more than one stage, and a pipeline that is
   the operand of another is a part of it. *)
let one_stage_pipelines_in syntax_tree =
  let pipelines = pipelines_in syntax_tree in
  let operands =
    Source.Extent.Set.of_list
      (List.map pipelines ~f:(fun (_, operand) -> extent_of operand))
  in
  List.filter_map pipelines ~f:(fun (pipeline, operand) ->
    Option.some_if
      (not (is_pipeline operand || Set.mem operands (extent_of pipeline)))
      (extent_of pipeline, (Idiom.Form.One_stage_pipeline, "|>")))
;;

(* A named function's body starts at no depth, and each anonymous function within it goes
   one deeper. *)
type nesting =
  { mutable depth : int
  ; anonymous_forms : (Source.Extent.t * (Idiom.Form.t * string)) Queue.t
  }

let least_depth = 3

let situate nesting ~depth traverse =
  let outer_depth = nesting.depth in
  nesting.depth <- depth;
  traverse ();
  nesting.depth <- outer_depth
;;

(* A let that defines a function names it, and its body starts again at no depth. *)
let name nesting (self : Ast_iterator.iterator) definition =
  let body = definition_of definition.pvb_expr in
  match body.pexp_desc with
  | Pexp_function _ ->
    situate nesting ~depth:0 (fun () ->
      self.pat self definition.pvb_pat;
      base.expr self body)
  | _ -> base.value_binding self definition
;;

(* An anonymous function is an eta-expansion by its shape, and nested by its depth. A
   named function may keep its parameter, so only an anonymous one is an eta-expansion. *)
let anonymous_forms_of ~depth parameters body =
  List.filter_opt
    [ Option.some_if (is_eta_expansion parameters body) (Idiom.Form.Eta_expansion, "fun")
    ; Option.some_if (depth = least_depth) (Idiom.Form.Nested_anonymous_function, "fun")
    ]
;;

let enter nesting (self : Ast_iterator.iterator) expression =
  match expression.pexp_desc with
  | Pexp_function (parameters, _, body) ->
    let depth = nesting.depth + 1 in
    List.iter (anonymous_forms_of ~depth parameters body) ~f:(fun form ->
      Queue.enqueue nesting.anonymous_forms (extent_of expression, form));
    situate nesting ~depth (fun () -> base.expr self expression)
  | _ -> base.expr self expression
;;

let anonymous_forms_in syntax_tree =
  let nesting = { depth = 0; anonymous_forms = Queue.create () } in
  iterate { base with expr = enter nesting; value_binding = name nesting } syntax_tree;
  Queue.to_list nesting.anonymous_forms
;;

let forms_in syntax_tree =
  Source.Extent.Map.of_alist_multi
    (List.concat
       [ local_forms_in syntax_tree
       ; one_stage_pipelines_in syntax_tree
       ; anonymous_forms_in syntax_tree
       ])
;;
