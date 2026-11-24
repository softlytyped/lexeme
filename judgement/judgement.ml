open! Core
module Codomain = Binding.Codomain
module Reading = Grammar.Reading
module Rule = Diagnostic.Rule

module Limits = struct
  type t =
    { note : int
    ; warning : int
    ; error : int
    ; test_allowance : int [@default 5]
    }
  [@@deriving of_sexp]

  let is_ordered limits =
    0 < limits.note
    && limits.note <= limits.warning
    && limits.warning <= limits.error
    && 0 <= limits.test_allowance
  ;;

  let t_of_sexp sexp =
    let limits = t_of_sexp sexp in
    if is_ordered limits
    then limits
    else
      of_sexp_error
        "expected 0 < note <= warning <= error, and no negative allowance"
        sexp
  ;;

  let default = { note = 10; warning = 20; error = 30; test_allowance = 5 }

  let allowance_of limits (role : Binding.Role.t) =
    match role with
    | Test -> limits.test_allowance
    | Variable | Function _ | Field _ | Type | Constructor -> 0
  ;;

  (* The bounds past which a definition earns each severity, the most severe first. *)
  let bounds_of limits role =
    let allowance = allowance_of limits role in
    [ Diagnostic.Severity.Error, limits.error
    ; Warning, limits.warning
    ; Note, limits.note
    ]
    |> List.map ~f:(fun (severity, bound) -> severity, bound + allowance)
  ;;
end

type context =
  { lexicon : Lexicon.t
  ; grammar : Grammar.t
  ; limits : Limits.t
  ; exemptions : String.Set.t
  }

type declaration =
  { binding : Binding.t
  ; singulars : string list
  ; diagnostics : Diagnostic.t list
  }

type scope =
  { findings : Diagnostic.t list
  ; decisions : Decision.t list
  ; notations : Notation.t list
  }

type t =
  | Declaration of declaration
  | Scope of scope

let expect readings phrases rule =
  let is_read phrase = List.mem readings phrase ~equal:Reading.equal in
  if List.exists phrases ~f:is_read then [] else [ Rule.Name rule ]
;;

let answer_rules_of codomain =
  match codomain with
  | Codomain.Boolean -> []
  | Optional_boolean -> [ Rule.Answer Optional_boolean ]
  | Fallible_boolean -> [ Answer Fallible_boolean ]
  | Unit spelling | Other_type spelling -> [ Answer (Wrong spelling) ]
  | Unknown -> [ Answer Unknown ]
;;

(* A function's name says what it gives: a predicate for a Boolean, a verb for nothing,
   and otherwise a verb or a name for what it returns. *)
let rules_of_function readings codomain =
  match codomain with
  | Codomain.Boolean -> [ Rule.Name Predicate ]
  | Unit _ -> expect readings [ Verb_phrase ] Command
  | Optional_boolean | Fallible_boolean | Other_type _ | Unknown ->
    expect readings [ Verb_phrase; Nominal ] Function
;;

let rules_of_field readings codomain =
  match codomain with
  | Codomain.Boolean -> [ Rule.Name Predicate ]
  | Optional_boolean | Fallible_boolean | Unit _ | Other_type _ | Unknown ->
    expect readings [ Noun_phrase ] Field
;;

(* A question asks for a Boolean, so its rules concern what it gives. *)
let typed_rules_of name readings codomain ~judge =
  match Grammar.Name.form_of name with
  | Question -> expect readings [ Predicate ] Function @ answer_rules_of (force codomain)
  | Statement -> judge readings (force codomain)
;;

let phrase_rules_of binding name readings =
  match Binding.role_of binding with
  | Variable -> expect readings [ Noun_phrase ] Variable
  | Function codomain -> typed_rules_of name readings codomain ~judge:rules_of_function
  | Field codomain -> typed_rules_of name readings codomain ~judge:rules_of_field
  | Type -> expect readings [ Noun_phrase ] Type
  | Constructor -> expect readings [ Noun_phrase ] Constructor
  | Test -> []
;;

(* A single letter passes when a comment on its line follows it directly and reads
   "lexeme: allow-letter" and then a reason. *)
let has_letter_exception binding =
  Option.exists (Binding.comment_of binding) ~f:(fun body ->
    match String.chop_prefix (String.strip body) ~prefix:"lexeme: allow-letter " with
    | None -> false
    | Some reason -> not (String.is_empty (String.strip reason) || String.mem body '\n'))
;;

(* The letter [t] names a module's main type. *)
let letter_rules_of binding =
  match Binding.role_of binding, Binding.name_of binding with
  | Type, "t" -> []
  | _ -> if has_letter_exception binding then [] else [ Rule.Name Single_letter ]
;;

let word_rules_of binding name readings =
  if Grammar.Name.is_letter name
  then letter_rules_of binding
  else phrase_rules_of binding name readings
;;

let name_rules_of context binding name readings =
  if Set.mem context.exemptions (Binding.name_of binding)
  then []
  else word_rules_of binding name readings
;;

let length_rules_of limits binding =
  let rule_of lines =
    Limits.bounds_of limits (Binding.role_of binding)
    |> List.find ~f:(fun (_, bound) -> lines > bound)
    |> Option.map ~f:(fun (severity, bound) ->
      Rule.Function_length { lines; bound; severity })
  in
  Option.to_list (Option.bind (Binding.lines_of binding) ~f:rule_of)
;;

let singulars_of grammar binding name readings =
  match Binding.role_of binding with
  | Function _ when List.mem readings Reading.Nominal ~equal:Reading.equal ->
    Grammar.singulars_of grammar name
  | Variable | Function _ | Field _ | Type | Constructor | Test -> []
;;

let declaration_of context binding =
  let name = Grammar.Name.of_spelling (Binding.name_of binding) in
  let readings = Grammar.readings_of context.grammar name in
  let rules =
    name_rules_of context binding name readings @ length_rules_of context.limits binding
  in
  { binding
  ; singulars = singulars_of context.grammar binding name readings
  ; diagnostics = List.map rules ~f:(Diagnostic.of_binding binding)
  }
;;

let form_rules_of = function
  | Decision.Form.Boolean_match | Tuple_match 1 -> [ Rule.Branch Boolean_match ]
  | Tuple_match count when count > 1 -> [ Branch (Boolean_tuple count) ]
  | Conditional | Tuple_match _ | Other_match -> []
;;

let placement_rules_of decision =
  match Decision.placement_of decision, Decision.nesting_of decision with
  | Late_position, Nested_position -> [ Rule.Branch Late_guard; Branch Nested_boolean ]
  | Late_position, Outer_position -> [ Rule.Branch Late_guard ]
  | Proper_position, Nested_position -> [ Rule.Branch Nested_boolean ]
  | Proper_position, Outer_position -> []
;;

let decide decision =
  let rules = form_rules_of (Decision.form_of decision) @ placement_rules_of decision in
  let findings = List.map rules ~f:(Diagnostic.of_decision decision) in
  Scope { findings; decisions = [ decision ]; notations = [] }
;;

let scope_with findings = Scope { findings; decisions = []; notations = [] }

let lines_of sites =
  List.map sites ~f:(fun site -> Int.to_string (Source.Site.line_of site))
  |> String.concat ~sep:", "
;;

let access field_access = scope_with [ Diagnostic.of_access field_access ]
let is_plain link = Chain.Link.Form.equal (Chain.Link.form_of link) Plain_binding

let pipeline_rules_of links =
  List.filter_map links ~f:(fun link ->
    match Chain.Link.definition_of link with
    | Pipeline -> Some (link, Rule.Chain Let_pipeline)
    | Other_expression -> None)
;;

(* A link depends on a bind when it binds, or when its definition reads a name that
   depends on a bind. *)
let is_dependent dependents link =
  match Chain.Link.form_of link with
  | Monad_binding -> true
  | Plain_binding -> List.exists (Chain.Link.reads_of link) ~f:(Set.mem dependents)
;;

let with_names dependents link =
  Set.union dependents (String.Set.of_list (Chain.Link.names_of link))
;;

(* A plain let after the first bind that depends on no bind could come before them all. *)
let late_lets_of links =
  List.drop_while links ~f:is_plain
  |> List.folding_map ~init:String.Set.empty ~f:(fun dependents link ->
    if is_dependent dependents link
    then with_names dependents link, None
    else dependents, Some (link, Rule.Chain Late_let))
  |> List.filter_opt
;;

let has_plain_ends links =
  match links, List.last links with
  | first :: _, Some last -> is_plain first && is_plain last
  | [], _ | _, None -> false
;;

(* A lone bind stands between plain lets when its chain opens and closes with them. *)
let lone_binds_of links =
  match List.filter links ~f:(Fn.non is_plain) with
  | [ bind ] when has_plain_ends links -> [ bind, Rule.Chain Lone_bind ]
  | _ -> []
;;

let chain links =
  let links = Chain.links_of links in
  List.concat [ pipeline_rules_of links; late_lets_of links; lone_binds_of links ]
  |> List.map ~f:(fun (link, rule) -> Diagnostic.of_link link rule)
  |> scope_with
;;

let sequence notation = Scope { findings = []; decisions = []; notations = [ notation ] }

let several_conditionals_of binding conditionals =
  match conditionals with
  | [] | [ _ ] -> []
  | _ :: _ :: _ ->
    let lines = lines_of (List.map conditionals ~f:Decision.site_of) in
    let rule = Rule.Branch (Several_conditionals lines) in
    [ Diagnostic.of_binding binding rule ]
;;

let lone_matches_of conditionals boolean_matches =
  match conditionals, boolean_matches with
  | [], [ lone_match ] ->
    [ Diagnostic.of_decision lone_match (Branch Lone_boolean_match) ]
  | _ -> []
;;

(* A function body keeps at most one if-then-else, which is how it writes a lone
   condition. *)
let body_rules_of binding (decisions : Decision.t list) =
  let of_form form =
    List.filter decisions ~f:(fun decision ->
      Decision.Form.equal (Decision.form_of decision) form)
  in
  let conditionals = of_form Conditional in
  several_conditionals_of binding conditionals
  @ lone_matches_of conditionals (of_form Boolean_match)
;;

let has_both_forms uses =
  let forms = List.map uses ~f:Notation.form_of in
  List.mem forms Infix_operator ~equal:Notation.Form.equal
  && List.mem forms Let_syntax ~equal:Notation.Form.equal
;;

(* A function writes each of a monad's operations in one form. *)
let notation_rules_of binding notations =
  List.filter_map Notation.Operation.all ~f:(fun operation ->
    let uses =
      List.filter notations ~f:(fun use ->
        Notation.Operation.equal (Notation.operation_of use) operation)
    in
    let lines = lines_of (List.map uses ~f:Notation.site_of) in
    Option.some_if
      (has_both_forms uses)
      (Diagnostic.of_binding binding (Mixed_notation { operation; lines })))
;;

let with_declaration declarations binding =
  Map.set declarations ~key:(Binding.name_of binding) ~data:binding
;;

(* We seed a scope with its first declaration of each name, so that a plural meets a
   singular declared after it. Reading the scope in source order then respects each later
   shadow. *)
let first_declarations_of children =
  List.fold_right children ~init:String.Map.empty ~f:(fun child declarations ->
    match child with
    | Scope _ -> declarations
    | Declaration { binding; _ } -> with_declaration declarations binding)
;;

let is_function binding =
  match Binding.role_of binding with
  | Function _ -> true
  | Variable | Field _ | Type | Constructor | Test -> false
;;

let lift_notes_of declarations binding singulars =
  List.filter_map singulars ~f:(Map.find declarations)
  |> List.filter ~f:is_function
  |> List.map ~f:(fun singular -> Diagnostic.of_binding binding (Plural_lift singular))
;;

let findings_in declarations = function
  | Scope { findings; _ } -> declarations, findings
  | Declaration { binding; singulars; diagnostics } ->
    let notes = lift_notes_of declarations binding singulars in
    with_declaration declarations binding, diagnostics @ notes
;;

(* A scope's decisions and uses of a monad belong to the function body that holds it. *)
let body_facts_of children ~facts_of =
  List.concat_map children ~f:(function
    | Declaration _ -> []
    | Scope scope -> facts_of scope)
;;

let scope_of children =
  let declarations = first_declarations_of children in
  let findings = List.folding_map children ~init:declarations ~f:findings_in in
  { findings = List.concat findings
  ; decisions = body_facts_of children ~facts_of:(fun scope -> scope.decisions)
  ; notations = body_facts_of children ~facts_of:(fun scope -> scope.notations)
  }
;;

let define context binding children =
  let declaration = declaration_of context binding in
  let body = scope_of children in
  let body_rules =
    body_rules_of binding body.decisions @ notation_rules_of binding body.notations
  in
  let diagnostics = List.concat [ declaration.diagnostics; body_rules; body.findings ] in
  Declaration { declaration with diagnostics }
;;

let algebra_in context =
  { Syntax.declare = (fun binding -> Declaration (declaration_of context binding))
  ; decide
  ; access
  ; chain
  ; sequence
  ; enclose = (fun children -> Scope (scope_of children))
  ; define = define context
  }
;;

let algebra_of lexicon ~limits ~exemptions =
  let grammar = Grammar.of_lexicon lexicon in
  algebra_in { lexicon; grammar; limits; exemptions }
;;

let diagnostics_of judgement =
  let diagnostics =
    match judgement with
    | Declaration { diagnostics; _ } -> diagnostics
    | Scope { findings; _ } -> findings
  in
  let site_of = Diagnostic.site_of in
  List.stable_sort diagnostics ~compare:(Comparable.lift Source.Site.compare ~f:site_of)
;;
