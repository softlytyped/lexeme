open! Core
open Ocaml_parsing
open Parsetree

let base = Ast_iterator.default_iterator

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

(* Returns the names that a selection finds at the patterns and expressions beneath a
   node. *)
let names_in traversal ~pattern ~expression =
  let names = Queue.create () in
  let gather selection node = Option.iter (selection node) ~f:(Queue.enqueue names) in
  let iterator =
    { base with
      pat =
        (fun self node ->
          gather pattern node;
          base.pat self node)
    ; expr =
        (fun self node ->
          gather expression node;
          base.expr self node)
    }
  in
  traversal iterator;
  String.Set.of_list (Queue.to_list names)
;;

let name_bound_at pattern =
  match pattern.ppat_desc with
  | Ppat_var name | Ppat_alias (_, name) -> Some name.txt
  | _ -> None
;;

let identifier_at expression =
  match expression.pexp_desc with
  | Pexp_ident { txt = Lident name; _ } -> Some name
  | _ -> None
;;

let names_bound_by pattern =
  let traversal (iterator : Ast_iterator.iterator) = iterator.pat iterator pattern in
  names_in traversal ~pattern:name_bound_at ~expression:(Fn.const None)
;;

let is_free_of work expression =
  let traversal (iterator : Ast_iterator.iterator) = iterator.expr iterator expression in
  Set.are_disjoint
    work
    (names_in traversal ~pattern:(Fn.const None) ~expression:identifier_at)
;;

(* A local function, an alias and a constant do no work, though any other definition does. *)
let work_of definitions =
  List.filter definitions ~f:(fun definition ->
    match definition.pvb_expr.pexp_desc with
    | Pexp_function _ | Pexp_ident _ | Pexp_constant _ -> false
    | _ -> true)
  |> List.map ~f:(fun definition -> names_bound_by definition.pvb_pat)
  |> String.Set.union_list
;;

(* The work that the next link of a body's chain does, and the rest of the chain. *)
let link_of expression =
  match expression.pexp_desc with
  | Pexp_let (_, _, definitions, rest) -> Some (work_of definitions, rest)
  | Pexp_letop { let_ = first; ands; body } ->
    let patterns = List.map (first :: ands) ~f:(fun operation -> operation.pbop_pat) in
    Some (String.Set.union_list (List.map patterns ~f:names_bound_by), body)
  | Pexp_sequence (_, rest)
  | Pexp_constraint (rest, _, _)
  | Pexp_newtype (_, _, rest)
  | Pexp_open (_, rest)
  | Pexp_extension (_, PStr [ { pstr_desc = Pstr_eval (rest, _); _ } ]) ->
    Some (String.Set.empty, rest)
  | _ -> None
;;

(* A guard is late when work comes before it, and neither its condition nor one of its
   branches uses that work. *)
let is_late_guard work expression =
  let is_late condition branches =
    (not (Set.is_empty work))
    && is_free_of work condition
    && List.exists branches ~f:(is_free_of work)
  in
  match expression.pexp_desc with
  | Pexp_ifthenelse (condition, consequent, alternative) ->
    is_late condition (consequent :: Option.to_list alternative)
  | Pexp_match (scrutinee, cases) ->
    is_late scrutinee (List.map cases ~f:(fun case -> case.pc_rhs))
  | _ -> false
;;

let rec late_guards_on work expression =
  let statements =
    match expression.pexp_desc with
    | Pexp_sequence (statement, _) -> [ expression; statement ]
    | _ -> [ expression ]
  in
  let guards = List.filter statements ~f:(is_late_guard work) in
  let extents = List.map guards ~f:(fun guard -> Session.extent_of guard.pexp_loc) in
  match link_of expression with
  | Some (links, rest) -> extents @ late_guards_on (Set.union work links) rest
  | None -> extents
;;

let late_guards_in parsetree =
  let guards = Queue.create () in
  let gather expression =
    match expression.pexp_desc with
    | Pexp_function (_, _, Pfunction_body body) ->
      Queue.enqueue_all guards (late_guards_on String.Set.empty body)
    | _ -> ()
  in
  let iterator =
    { base with
      expr =
        (fun self node ->
          gather node;
          base.expr self node)
    }
  in
  (match parsetree with
   | `Interface items -> iterator.signature iterator items
   | `Implementation items -> iterator.structure iterator items);
  Source.Extent.Set.of_list (Queue.to_list guards)
;;
