open! Core
module Codomain = Binding.Codomain
module Reading = Grammar.Reading
module Rule = Diagnostic.Rule

module Limits = struct
  type t =
    { soft : int
    ; hard : int
    ; reject : int
    }
  [@@deriving of_sexp]

  let t_of_sexp sexp =
    let limits = t_of_sexp sexp in
    match
      0 < limits.soft && limits.soft <= limits.hard && limits.hard <= limits.reject
    with
    | true -> limits
    | false -> of_sexp_error "expected 0 < soft <= hard <= reject" sexp
  ;;

  let default = { soft = 10; hard = 20; reject = 30 }
end

type context =
  { grammar : Grammar.t
  ; limits : Limits.t
  ; exemptions : String.Set.t
  }

type t =
  | Declaration of
      { binding : Binding.t
      ; singulars : string list
      ; diagnostics : Diagnostic.t list
      }
  | Scope of Diagnostic.t list

let has reading readings = List.mem readings reading ~equal:Reading.equal

let singulars_of (binding : Binding.t) readings =
  match binding.role with
  | Variable -> []
  | Function _ ->
    List.concat_map readings ~f:(function
      | Reading.Nominal singulars -> singulars
      | Noun_phrase | Verb_phrase | Predicate -> [])
;;

(* A single letter passes when a comment on its line follows it directly and reads
   "lexeme: allow-letter" and then a reason. *)
let is_letter_allowed (binding : Binding.t) =
  Option.exists binding.comment ~f:(fun body ->
    match String.chop_prefix (String.strip body) ~prefix:"lexeme: allow-letter " with
    | None -> false
    | Some reason -> not (String.is_empty (String.strip reason) || String.mem body '\n'))
;;

let answer_rule_of codomain =
  match codomain with
  | Codomain.Boolean -> None
  | Optional_boolean -> Some (Rule.Answer Optional)
  | Fallible_boolean -> Some (Answer Fallible)
  | Unit spelling | Other spelling -> Some (Answer (Wrong spelling))
  | Unknown -> Some (Answer Unknown)
;;

(* A name that asks no question says what its function gives: a predicate for a Boolean, a
   verb for nothing, and otherwise a verb or a name for what it returns. *)
let statement_rule_of readings codomain =
  let is_nominal =
    List.exists readings ~f:(function
      | Reading.Nominal _ -> true
      | Noun_phrase | Verb_phrase | Predicate -> false)
  in
  match codomain with
  | Codomain.Boolean -> Some (Rule.Name Predicate)
  | Unit _ -> Option.some_if (not (has Verb_phrase readings)) (Rule.Name Command)
  | Optional_boolean | Fallible_boolean | Other _ | Unknown ->
    Option.some_if (not (has Verb_phrase readings || is_nominal)) (Rule.Name Function)
;;

let function_rules_of name readings codomain =
  match Grammar.Name.has_auxiliary name with
  | true ->
    let malformation =
      Option.some_if (not (has Predicate readings)) (Rule.Name Function)
    in
    List.filter_opt [ malformation; answer_rule_of codomain ]
  | false -> Option.to_list (statement_rule_of readings codomain)
;;

let phrase_rules_of (binding : Binding.t) name readings =
  match binding.role with
  | Variable ->
    Option.to_list (Option.some_if (not (has Noun_phrase readings)) (Rule.Name Variable))
  | Function codomain -> function_rules_of name readings (force codomain)
;;

let name_rules_of context (binding : Binding.t) name readings =
  match Set.mem context.exemptions binding.name with
  | true -> []
  | false ->
    (match Grammar.Name.is_letter name with
     | true ->
       Option.to_list
         (Option.some_if (not (is_letter_allowed binding)) (Rule.Name Single_letter))
     | false -> phrase_rules_of binding name readings)
;;

let length_rule_of (limits : Limits.t) (binding : Binding.t) =
  let%bind.Option lines = binding.lines in
  [ Diagnostic.Limit.Reject, limits.reject; Hard, limits.hard; Soft, limits.soft ]
  |> List.find ~f:(fun (_, bound) -> lines > bound)
  |> Option.map ~f:(fun (limit, bound) -> Rule.Function_length { lines; limit; bound })
;;

let declare context (binding : Binding.t) =
  let name = Grammar.Name.of_spelling binding.name in
  let readings = Grammar.readings_of context.grammar name in
  let rules =
    name_rules_of context binding name readings
    @ Option.to_list (length_rule_of context.limits binding)
  in
  let diagnostics = List.map rules ~f:(Diagnostic.of_binding binding) in
  Declaration { binding; singulars = singulars_of binding readings; diagnostics }
;;

let diagnostics_in = function
  | Declaration { diagnostics; _ } | Scope diagnostics -> diagnostics
;;

let declare_in declarations (binding : Binding.t) =
  Map.set declarations ~key:binding.name ~data:binding
;;

(* We seed a scope with its first declaration of each name, so that a plural meets a
   singular declared after it. Reading the scope in source order then respects each later
   shadow. *)
let first_declarations_of children =
  List.fold_right children ~init:String.Map.empty ~f:(fun child declarations ->
    match child with
    | Scope _ -> declarations
    | Declaration { binding; _ } -> declare_in declarations binding)
;;

let lift_notes_of declarations binding singulars =
  List.filter_map singulars ~f:(fun spelling ->
    match Map.find declarations spelling with
    | Some ({ Binding.role = Function _; _ } as singular) ->
      Some (Diagnostic.of_binding binding (Plural_lift singular))
    | Some { role = Variable; _ } | None -> None)
;;

let enclose children =
  List.folding_map
    children
    ~init:(first_declarations_of children)
    ~f:(fun declarations -> function
    | Scope diagnostics -> declarations, diagnostics
    | Declaration { binding; singulars; diagnostics } ->
      ( declare_in declarations binding
      , diagnostics @ lift_notes_of declarations binding singulars ))
  |> List.concat
  |> Scope
;;

let algebra_of lexicon ~limits ~exemptions =
  let context = { grammar = Grammar.of_lexicon lexicon; limits; exemptions } in
  { Syntax.declare = declare context; enclose }
;;

let diagnostics_of judgement =
  List.stable_sort
    (diagnostics_in judgement)
    ~compare:
      (Comparable.lift Source.Site.compare ~f:(fun (diagnostic : Diagnostic.t) ->
         diagnostic.site))
;;
