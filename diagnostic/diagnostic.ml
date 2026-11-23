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

module Limit = struct
  type t =
    | Soft
    | Hard
    | Reject

  let severity_of = function
    | Soft -> Severity.Note
    | Hard -> Warning
    | Reject -> Error
  ;;

  let to_string = function
    | Soft -> "the soft limit"
    | Hard -> "the hard limit"
    | Reject -> "the limit"
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

    let code_of = function
      | Single_letter -> "single-letter"
      | Variable -> "variable-name"
      | Function -> "function-name"
      | Predicate -> "predicate-name"
      | Command -> "command-name"
    ;;

    let message_of = function
      | Single_letter ->
        "use a longer name or add a lexeme: allow-letter comment with a reason"
      | Variable -> "expected a noun phrase"
      | Function -> "expected a root verb phrase, a prepositional name or a predicate"
      | Predicate ->
        "expected a predicate such as is_empty or has_entries for a Boolean result"
      | Command -> "expected a root verb phrase for a function that gives nothing"
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

  type t =
    | Name of Name.t
    | Answer of Answer.t
    | Plural_lift of Binding.t
    | Function_length of
        { lines : int
        ; limit : Limit.t
        ; bound : int
        }

  let severity_of = function
    | Name _ -> Severity.Error
    | Answer answer -> Answer.severity_of answer
    | Plural_lift _ -> Note
    | Function_length { limit; _ } -> Limit.severity_of limit
  ;;

  let code_of = function
    | Name name -> Name.code_of name
    | Answer answer -> Answer.code_of answer
    | Plural_lift _ -> "plural-lift"
    | Function_length _ -> "function-length"
  ;;

  let message_of = function
    | Name name -> Name.message_of name
    | Answer answer -> Answer.message_of answer
    | Plural_lift (singular : Binding.t) ->
      sprintf
        "check whether this is a map-like lift of %s (line %d)"
        singular.name
        singular.site.position.line
    | Function_length { lines; limit; bound } ->
      sprintf "%d lines of code, past %s of %d" lines (Limit.to_string limit) bound
  ;;
end

type t =
  { site : Source.Site.t
  ; subject : string
  ; rule : Rule.t
  }

let create ~site ~subject rule = { site; subject; rule }

let of_binding (binding : Binding.t) rule =
  create ~site:binding.site ~subject:binding.name rule
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
