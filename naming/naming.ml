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
  { declaration : (binding * string list) option
  ; diagnostics : diagnostic list
  }

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
     | Function ->
       (match Grammar.function_of grammar phrase with
        | Some singulars -> singulars, []
        | None -> [], [ Function_name ]))
;;

let judgement_of binding phrase =
  match binding.role, Grammar.is_predicate phrase with
  | Function, true ->
    (match force binding.result with
     | Boolean -> []
     | Optional_boolean -> [ Optional_result ]
     | Fallible_boolean -> [ Fallible_result ]
     | Other spelling -> [ Wrong_result spelling ]
     | Unknown -> [ Unknown_result ])
  | _ -> []
;;

let algebra_of lexicon =
  let grammar = Grammar.grammar_of lexicon in
  let bind binding =
    let phrase = Grammar.read binding.name in
    let singulars, rules = reading_of grammar binding phrase in
    let diagnostics =
      List.map (rules @ judgement_of binding phrase) ~f:(fun rule -> { binding; rule })
    in
    { declaration = Some (binding, singulars); diagnostics }
  in
  let enclose children =
    let declare declarations child =
      match child.declaration with
      | None -> declarations
      | Some (binding, _) -> Map.set declarations ~key:binding.name ~data:binding
    in
    (* We first make a later sibling available for a forward pair. Each subsequent binding
       shadows the previous one as we walk the scope in source order. *)
    let declarations =
      List.fold_right children ~init:String.Map.empty ~f:(Fn.flip declare)
    in
    let diagnostics =
      List.folding_map children ~init:declarations ~f:(fun declarations child ->
        match child.declaration with
        | None -> declarations, child.diagnostics
        | Some (binding, singulars) ->
          let notes =
            List.filter_map singulars ~f:(fun name ->
              let%bind.Option singular = Map.find declarations name in
              let%map.Option () = Option.some_if (equal_role singular.role Function) () in
              { binding; rule = Plural_lift singular })
          in
          declare declarations child, child.diagnostics @ notes)
    in
    { declaration = None; diagnostics = List.concat diagnostics }
  in
  { bind; enclose }
;;

let diagnostics_of judgement =
  let location_of diagnostic = diagnostic.binding.line, diagnostic.binding.column in
  List.stable_sort judgement.diagnostics ~compare:(fun first second ->
    [%compare: int * int] (location_of first) (location_of second))
;;
