open! Core

module Severity = struct
  type t =
    | Note
    | Warning
    | Error
  [@@deriving equal]

  let to_string = function
    | Note -> "note"
    | Warning -> "warning"
    | Error -> "error"
  ;;
end

module Rule = struct
  module Name = struct
    type t =
      | Single_letter
      | Variable
      | Function
      | Predicate
      | Command
      | Field
      | Type
      | Constructor

    let code_of = function
      | Single_letter -> "single-letter"
      | Variable -> "variable-name"
      | Function -> "function-name"
      | Predicate -> "predicate-name"
      | Command -> "command-name"
      | Field -> "field-name"
      | Type -> "type-name"
      | Constructor -> "constructor-name"
    ;;

    let message_of = function
      | Single_letter ->
        "use a longer name or add a lexeme: allow-letter comment with a reason"
      | Variable | Field | Type -> "expected a noun phrase"
      | Function -> "expected a root verb phrase, a prepositional name or a predicate"
      | Predicate -> "expected a predicate such as is_empty or has_entries for a Boolean"
      | Command -> "expected a root verb phrase for a function that gives nothing"
      | Constructor -> "expected a noun phrase or an adjective phrase"
    ;;
  end

  module Answer = struct
    type t =
      | Optional
      | Fallible
      | Wrong of string
      | Unknown

    let severity_of = function
      | Optional | Fallible -> Severity.Note
      | Wrong _ | Unknown -> Error
    ;;

    let code_of answer =
      match severity_of answer with
      | Note -> "predicate-state"
      | Warning | Error -> "predicate-result"
    ;;

    let question_of answer alternative =
      sprintf
        "Are you sure %s is a meaningfully different state from %s? If so, you can \
         ignore this note."
        answer
        alternative
    ;;

    let message_of = function
      | Optional -> question_of "Some false" "None"
      | Fallible -> question_of "Ok false" "Error"
      | Wrong spelling -> "expected a Boolean result, found " ^ spelling
      | Unknown -> "could not determine the result type. Add a result annotation."
    ;;
  end

  module Branch = struct
    type t =
      | Boolean_match
      | Boolean_tuple of int
      | Lone_boolean_match
      | Several_ifs of Source.Position.t list
      | Late_guard

    let severity_of = function
      | Boolean_match | Late_guard -> Severity.Note
      | Lone_boolean_match | Several_ifs _ -> Warning
      | Boolean_tuple _ -> Error
    ;;

    let code_of = function
      | Boolean_match -> "boolean-match"
      | Boolean_tuple _ -> "boolean-tuple"
      | Lone_boolean_match -> "lone-boolean-match"
      | Several_ifs _ -> "if-else"
      | Late_guard -> "late-guard"
    ;;

    let lines_of positions =
      List.map positions ~f:(fun (position : Source.Position.t) ->
        Int.to_string position.line)
      |> String.concat ~sep:", "
    ;;

    let message_of = function
      | Boolean_match -> "could a variant name the cases of this Boolean?"
      | Boolean_tuple count ->
        sprintf "matches %d Booleans at once: parse, don't validate, with a variant" count
      | Lone_boolean_match ->
        "the one Boolean decision of its function reads as an if-else"
      | Several_ifs positions ->
        sprintf
          "has if-else on lines %s: keep at most one, and match the rest"
          (lines_of positions)
      | Late_guard ->
        "this precondition uses none of the work before it: could it come first?"
    ;;
  end

  type t =
    | Name of Name.t
    | Answer of Answer.t
    | Branch of Branch.t
    | Plural_lift of Binding.t
    | Function_length of
        { lines : int
        ; bound : int
        ; severity : Severity.t
        }

  let severity_of = function
    | Name _ -> Severity.Error
    | Answer answer -> Answer.severity_of answer
    | Branch branch -> Branch.severity_of branch
    | Plural_lift _ -> Note
    | Function_length { severity; _ } -> severity
  ;;

  let code_of = function
    | Name name -> Name.code_of name
    | Answer answer -> Answer.code_of answer
    | Branch branch -> Branch.code_of branch
    | Plural_lift _ -> "plural-lift"
    | Function_length _ -> "function-length"
  ;;

  let message_of = function
    | Name name -> Name.message_of name
    | Answer answer -> Answer.message_of answer
    | Branch branch -> Branch.message_of branch
    | Plural_lift (singular : Binding.t) ->
      sprintf
        "check whether this is a map-like lift of %s (line %d)"
        singular.name
        singular.site.position.line
    | Function_length { lines; bound; _ } ->
      sprintf "%d lines of code, more than %d" lines bound
  ;;
end

type t =
  { site : Source.Site.t
  ; subject : string
  ; rule : Rule.t
  }

let create ~site ~subject rule = { site; subject; rule }
let of_binding (binding : Binding.t) = create ~site:binding.site ~subject:binding.name

let of_decision (decision : Decision.t) =
  create ~site:decision.site ~subject:decision.keyword
;;

let severity_of diagnostic = Rule.severity_of diagnostic.rule

let to_string { site = { path; position = { line; column } }; subject; rule } =
  sprintf
    "%s:%d:%d: %s[%s] %s: %s"
    path
    line
    column
    (Severity.to_string (Rule.severity_of rule))
    (Rule.code_of rule)
    subject
    (Rule.message_of rule)
;;
