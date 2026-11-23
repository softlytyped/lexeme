open! Core
open Ocaml_parsing
open Parsetree

let base = Ast_iterator.default_iterator

let iterate (iterator : Ast_iterator.iterator) = function
  | `Interface items -> iterator.signature iterator items
  | `Implementation items -> iterator.structure iterator items
;;

let rec alternatives_of pattern =
  match pattern.ppat_desc with
  | Ppat_or (left, right) -> alternatives_of left @ alternatives_of right
  | Ppat_alias (body, _) | Ppat_constraint (body, _, _) -> alternatives_of body
  | _ -> [ pattern ]
;;

let is_boolean pattern =
  List.exists (alternatives_of pattern) ~f:(fun alternative ->
    match alternative.ppat_desc with
    | Ppat_construct ({ txt = Lident ("true" | "false"); _ }, None) -> true
    | _ -> false)
;;

let components_of pattern =
  match pattern.ppat_desc with
  | Ppat_tuple (components, _) -> Some (List.map components ~f:snd)
  | _ -> None
;;

(* A match on a tuple counts the components that some case matches as a Boolean. *)
let form_of_cases cases =
  let patterns = List.concat_map cases ~f:(fun case -> alternatives_of case.pc_lhs) in
  match List.transpose (List.filter_map patterns ~f:components_of) with
  | Some (_ :: _ as columns) ->
    Decision.Form.Tuple_match (List.count columns ~f:(List.exists ~f:is_boolean))
  | Some [] | None ->
    if List.exists patterns ~f:is_boolean then Boolean_match else Other_match
;;

(* Returns the names that a traversal adds as it goes. *)
let gather traverse =
  let names = Queue.create () in
  traverse (Queue.enqueue names);
  String.Set.of_list (Queue.to_list names)
;;

let names_bound_by pattern =
  gather (fun add ->
    let visit self node =
      (match node.ppat_desc with
       | Ppat_var name | Ppat_alias (_, name) -> add name.txt
       | _ -> ());
      base.pat self node
    in
    let iterator = { base with pat = visit } in
    iterator.pat iterator pattern)
;;

let identifiers_in expression =
  gather (fun add ->
    let visit self node =
      (match node.pexp_desc with
       | Pexp_ident { txt = Lident name; _ } -> add name
       | _ -> ());
      base.expr self node
    in
    let iterator = { base with expr = visit } in
    iterator.expr iterator expression)
;;

let does_avoid work expression = Set.are_disjoint work (identifiers_in expression)

let is_trivial expression =
  match expression.pexp_desc with
  | Pexp_function _ | Pexp_ident _ | Pexp_constant _ | Pexp_field _ -> true
  | _ -> false
;;

(* A local function, an alias, a constant and a projection do no work of their own, but
   carry the work that they use. Any other definition does work. *)
let work_of work definitions =
  List.filter definitions ~f:(fun definition ->
    (not (is_trivial definition.pvb_expr)) || not (does_avoid work definition.pvb_expr))
  |> List.map ~f:(fun definition -> names_bound_by definition.pvb_pat)
  |> String.Set.union_list
;;

let work_of_operations operations =
  List.map operations ~f:(fun operation -> names_bound_by operation.pbop_pat)
  |> String.Set.union_list
;;

(* The expression beneath a wrapper that does no work: the rest of a sequence, a
   constraint, a type, an open or an extension. *)
let tail_of expression =
  match expression.pexp_desc with
  | Pexp_sequence (_, tail)
  | Pexp_constraint (tail, _, _)
  | Pexp_newtype (_, _, tail)
  | Pexp_open (_, tail)
  | Pexp_extension (_, PStr [ { pstr_desc = Pstr_eval (tail, _); _ } ]) -> Some tail
  | _ -> None
;;

(* The work that the next link of a body's chain does, and the rest of the chain. *)
let link_of work expression =
  match expression.pexp_desc with
  | Pexp_let (_, _, definitions, rest) -> Some (work_of work definitions, rest)
  | Pexp_letop { let_ = first; ands = later_operations; body } ->
    Some (work_of_operations (first :: later_operations), body)
  | _ -> Option.map (tail_of expression) ~f:(fun tail -> String.Set.empty, tail)
;;

(* A guard clause is late when work comes before it, and neither its condition nor one of
   its branches uses that work. *)
let is_late work ~condition ~branches =
  (not (Set.is_empty work))
  && does_avoid work condition
  && List.exists branches ~f:(does_avoid work)
;;

let is_late_guard work expression =
  match expression.pexp_desc with
  | Pexp_ifthenelse (condition, consequent, alternative) ->
    is_late work ~condition ~branches:(consequent :: Option.to_list alternative)
  | Pexp_match (scrutinee, cases) ->
    is_late
      work
      ~condition:scrutinee
      ~branches:(List.map cases ~f:(fun case -> case.pc_rhs))
  | _ -> false
;;

let statements_of expression =
  match expression.pexp_desc with
  | Pexp_sequence (statement, _) -> [ expression; statement ]
  | _ -> [ expression ]
;;

let rec late_guards_on work expression =
  let guards = List.filter (statements_of expression) ~f:(is_late_guard work) in
  let extents = List.map guards ~f:(fun guard -> Session.extent_of guard.pexp_loc) in
  match link_of work expression with
  | Some (links, rest) -> extents @ late_guards_on (Set.union work links) rest
  | None -> extents
;;

let guards_of_function expression =
  match expression.pexp_desc with
  | Pexp_function (_, _, Pfunction_body body) -> late_guards_on String.Set.empty body
  | _ -> []
;;

let late_guards_in syntax_tree =
  let guards = Queue.create () in
  let visit self expression =
    Queue.enqueue_all guards (guards_of_function expression);
    base.expr self expression
  in
  iterate { base with expr = visit } syntax_tree;
  Source.Extent.Set.of_list (Queue.to_list guards)
;;

let is_boolean_form = function
  | Decision.Form.Boolean_match -> true
  | Tuple_match count -> count > 0
  | Conditional | Other_match -> false
;;

(* The Boolean decisions that enclose a traversal's expression within its function body,
   and the Boolean matches that we have found within another. *)
type nesting =
  { mutable depth : int
  ; nested_matches : Source.Extent.t Queue.t
  }

let situate nesting ~depth traverse =
  let outer_depth = nesting.depth in
  nesting.depth <- depth;
  traverse ();
  nesting.depth <- outer_depth
;;

let follow nesting (self : Ast_iterator.iterator) expression scrutinee cases =
  let form = form_of_cases cases in
  if nesting.depth > 0 && Decision.Form.equal form Boolean_match
  then Queue.enqueue nesting.nested_matches (Session.extent_of expression.pexp_loc);
  self.expr self scrutinee;
  situate
    nesting
    ~depth:(nesting.depth + Bool.to_int (is_boolean_form form))
    (fun () -> List.iter cases ~f:(self.case self))
;;

let fork nesting (self : Ast_iterator.iterator) condition branches =
  self.expr self condition;
  situate nesting ~depth:(nesting.depth + 1) (fun () ->
    List.iter branches ~f:(self.expr self))
;;

(* A function's body starts at no depth, or within its own match where that is on a
   Boolean. *)
let depth_of_body = function
  | Pfunction_cases (cases, _, _) -> Bool.to_int (is_boolean_form (form_of_cases cases))
  | Pfunction_body _ -> 0
;;

let track nesting (self : Ast_iterator.iterator) expression =
  match expression.pexp_desc with
  | Pexp_ifthenelse (condition, consequent, alternative) ->
    fork nesting self condition (consequent :: Option.to_list alternative)
  | Pexp_match (scrutinee, cases) -> follow nesting self expression scrutinee cases
  | Pexp_function (_, _, body) ->
    situate nesting ~depth:(depth_of_body body) (fun () -> base.expr self expression)
  | _ -> base.expr self expression
;;

let nested_booleans_in syntax_tree =
  let nesting = { depth = 0; nested_matches = Queue.create () } in
  iterate { base with expr = track nesting } syntax_tree;
  Source.Extent.Set.of_list (Queue.to_list nesting.nested_matches)
;;
