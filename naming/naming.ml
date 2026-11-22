open! Core
open Binding_syntax

type rule =
  | Single_letter
  | Variable_name
  | Function_name
  | Plural_lift of binding
  | Optional_result
  | Fallible_result
  | Wrong_result of string
  | Unknown_result

type diagnostic =
  { binding : binding
  ; rule : rule
  }

let is_error diagnostic =
  match diagnostic.rule with
  | Plural_lift _ | Optional_result | Fallible_result -> false
  | Single_letter | Variable_name | Function_name | Wrong_result _ | Unknown_result ->
    true
;;

let format diagnostic =
  let { binding; rule } = diagnostic in
  let state_of answer alternative =
    ( "predicate-state"
    , sprintf
        "Are you sure %s is a meaningfully different state from %s? If so, you can \
         ignore this note."
        answer
        alternative )
  in
  let code, message =
    match rule with
    | Single_letter ->
      ( "single-letter"
      , "use a longer name or add a lexeme: allow-letter comment with a reason" )
    | Variable_name -> "variable-name", "expected a noun phrase"
    | Function_name ->
      "function-name", "expected a root verb phrase, a prepositional name or a predicate"
    | Optional_result -> state_of "Some false" "None"
    | Fallible_result -> state_of "Ok false" "Error"
    | Wrong_result spelling ->
      "predicate-result", "expected a Boolean result, found " ^ spelling
    | Unknown_result ->
      "predicate-result", "could not determine the result type. Add a result annotation."
    | Plural_lift singular ->
      ( "plural-lift"
      , sprintf
          "check whether this is a map-like lift of %s (line %d)"
          singular.name
          singular.line )
  in
  let severity =
    match is_error diagnostic with
    | true -> "error"
    | false -> "note"
  in
  sprintf
    "%s:%d:%d: %s[%s] %s: %s"
    binding.path
    binding.line
    binding.column
    severity
    code
    binding.name
    message
;;

type t =
  | Declaration of
      { binding : binding
      ; singulars : string list
      ; diagnostics : diagnostic list
      }
  | Scope of diagnostic list

let reading_of grammar binding phrase =
  match Grammar.is_letter binding.name, binding.letter_exception with
  | true, None -> [], [ Single_letter ]
  | true, Some _ -> [], []
  | false, _ ->
    (match binding.role with
     | Variable ->
       (match Grammar.is_noun grammar phrase with
        | true -> [], []
        | false -> [], [ Variable_name ])
     | Function _ ->
       (match Grammar.function_of grammar phrase with
        | Some singulars -> singulars, []
        | None -> [], [ Function_name ]))
;;

let judgement_of binding phrase =
  match binding.role, Grammar.is_predicate phrase with
  | Function result, true ->
    (match force result with
     | Boolean -> []
     | Optional_boolean -> [ Optional_result ]
     | Fallible_boolean -> [ Fallible_result ]
     | Other spelling -> [ Wrong_result spelling ]
     | Unknown -> [ Unknown_result ])
  | _ -> []
;;

let bind grammar binding =
  let phrase = Grammar.read binding.name in
  let singulars, rules = reading_of grammar binding phrase in
  let diagnostics =
    List.map (rules @ judgement_of binding phrase) ~f:(fun rule -> { binding; rule })
  in
  Declaration { binding; singulars; diagnostics }
;;

let diagnostics_in = function
  | Declaration { diagnostics; _ } | Scope diagnostics -> diagnostics
;;

let notes_of declarations binding singulars =
  List.filter_map singulars ~f:(fun name ->
    let%bind.Option singular = Map.find declarations name in
    match singular.role with
    | Variable -> None
    | Function _ -> Some { binding; rule = Plural_lift singular })
;;

let enclose children =
  let has_plural = function
    | Declaration { singulars = _ :: _; _ } -> true
    | _ -> false
  in
  match List.exists children ~f:has_plural with
  | false -> Scope (List.concat_map children ~f:diagnostics_in)
  | true ->
    let declare declarations binding =
      Map.set declarations ~key:binding.name ~data:binding
    in
    (* We seed the scope with its first declaration of each name for forward pairs.
       Reading the scope in source order then respects every subsequent shadow. *)
    let declarations =
      List.fold_right children ~init:String.Map.empty ~f:(fun child declarations ->
        match child with
        | Scope _ -> declarations
        | Declaration { binding; _ } -> declare declarations binding)
    in
    let diagnostics =
      List.folding_map children ~init:declarations ~f:(fun declarations -> function
        | Scope diagnostics -> declarations, diagnostics
        | Declaration { binding; singulars; diagnostics } ->
          ( declare declarations binding
          , diagnostics @ notes_of declarations binding singulars ))
    in
    Scope (List.concat diagnostics)
;;

let algebra_of lexicon = { bind = bind (Grammar.grammar_of lexicon); enclose }

let diagnostics_of judgement =
  let location_of diagnostic = diagnostic.binding.line, diagnostic.binding.column in
  List.stable_sort (diagnostics_in judgement) ~compare:(fun first second ->
    [%compare: int * int] (location_of first) (location_of second))
;;
