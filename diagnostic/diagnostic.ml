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

    let letter_message =
      "this name is a single letter, so use a longer name, or add a comment \"lexeme: \
       allow-letter\" that gives a reason"
    ;;

    let noun_phrase_message =
      "this name should be a noun phrase, such as matrix or start_of_line, because it \
       names a thing"
    ;;

    let function_message =
      "this name should be a verb phrase, a predicate, or a noun phrase with a \
       preposition, such as entries_of, because it names a function"
    ;;

    let predicate_message =
      "this name should be a predicate, such as is_empty or has_entries, because the \
       function returns a Boolean"
    ;;

    let command_message =
      "this name should be a verb phrase, because a function that returns nothing is a \
       command (command-query separation)"
    ;;

    let message_of = function
      | Single_letter -> letter_message
      | Variable | Field | Type | Constructor -> noun_phrase_message
      | Function -> function_message
      | Predicate -> predicate_message
      | Command -> command_message
    ;;
  end

  module Answer = struct
    type t =
      | Optional_boolean
      | Fallible_boolean
      | Wrong of string
      | Unknown

    let severity_of = function
      | Optional_boolean | Fallible_boolean -> Severity.Note
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
      | Optional_boolean -> question_of "Some false" "None"
      | Fallible_boolean -> question_of "Ok false" "Error"
      | Wrong spelling ->
        "this predicate should return a Boolean, but it returns " ^ spelling
      | Unknown -> "lexeme could not infer the result type, so add a type annotation"
    ;;
  end

  module Branch = struct
    type t =
      | Boolean_match
      | Boolean_tuple of int
      | Lone_boolean_match
      | Several_conditionals of string
      | Late_guard
      | Nested_boolean

    let severity_of = function
      | Boolean_match | Late_guard -> Severity.Note
      | Lone_boolean_match | Several_conditionals _ | Nested_boolean -> Warning
      | Boolean_tuple _ -> Error
    ;;

    let code_of = function
      | Boolean_match -> "boolean-match"
      | Boolean_tuple _ -> "boolean-tuple"
      | Lone_boolean_match -> "lone-boolean-match"
      | Several_conditionals _ -> "if-else"
      | Late_guard -> "late-guard"
      | Nested_boolean -> "nested-boolean"
    ;;

    let advice = "so parse, don't validate, with a variant type"

    let lone_match_message =
      "this match is the only Boolean condition of its function, so use if-then-else"
    ;;

    let late_guard_message =
      "this guard clause comes after unrelated work, so could it come first?"
    ;;

    let conditionals_message_of lines =
      sprintf
        "this function has if-then-else on lines %s, so use pattern matching for all but \
         one"
        lines
    ;;

    let message_of = function
      | Boolean_match -> "could a variant type name the cases of this Boolean?"
      | Boolean_tuple count ->
        sprintf "this match takes %d Booleans at once, %s" count advice
      | Lone_boolean_match -> lone_match_message
      | Several_conditionals lines -> conditionals_message_of lines
      | Late_guard -> late_guard_message
      | Nested_boolean -> "this conditional on Booleans lies within another, " ^ advice
    ;;
  end

  module Chain = struct
    type t =
      | Let_pipeline
      | Late_let
      | Lone_bind

    let code_of = function
      | Let_pipeline -> "let-pipeline"
      | Late_let -> "late-let"
      | Lone_bind -> "lone-bind"
    ;;

    let pipeline_message =
      "this let binds the result of a pipeline, so the chain mixes two styles; could the \
       pipeline end the chain as its result, or each of its steps have a let of its own?"
    ;;

    let late_let_message =
      "this let reads nothing that the binds before it give, so could it come before \
       them?"
    ;;

    let lone_bind_message =
      "this bind is the only one in its chain, and plain lets stand on both sides of it, \
       so could the chain open or close with it?"
    ;;

    let message_of = function
      | Let_pipeline -> pipeline_message
      | Late_let -> late_let_message
      | Lone_bind -> lone_bind_message
    ;;
  end

  (* Each idiom has a plainer form, which its message names. *)
  module Idiom = struct
    type t = Idiom.Form.t

    type description =
      { code : string
      ; severity : Severity.t
      ; message : string
      }

    let descriptions : (t * description) list =
      [ ( One_stage_pipeline
        , { code = "one-stage-pipeline"
          ; severity = Note
          ; message = "this pipeline has one stage, so could it be a plain application?"
          } )
      ; ( Boolean_branches
        , { code = "boolean-branches"
          ; severity = Warning
          ; message =
              "the branches of this if-then-else are true and false, so the expression \
               is its condition, or the negation of it"
          } )
      ; ( Eta_expansion
        , { code = "eta-expansion"
          ; severity = Note
          ; message =
              "this anonymous function applies a function to its parameter and does \
               nothing else, so could it be that function itself?"
          } )
      ; ( One_case_function
        , { code = "one-case-function"
          ; severity = Note
          ; message = "this function has one case, so could fun take its pattern?"
          } )
      ; ( Discard_without_type
        , { code = "untyped-ignore"
          ; severity = Warning
          ; message =
              "this ignore has no type annotation, so a partial application could pass \
               through it unseen; could it be ignore (value : type)?"
          } )
      ; ( Length_comparison
        , { code = "length-comparison"
          ; severity = Note
          ; message =
              "this compares a length with zero, so could is_empty, which for a list \
               stops at the first element, take its place?"
          } )
      ; ( Boolean_parameter
        , { code = "boolean-parameter"
          ; severity = Note
          ; message =
              "this parameter is a Boolean, so could a variant type name its cases?"
          } )
      ; ( Nested_anonymous_function
        , { code = "nested-function"
          ; severity = Note
          ; message =
              "this anonymous function is within two others, so could a let name one of \
               them?"
          } )
      ]
    ;;

    let description_of form =
      List.Assoc.find_exn descriptions form ~equal:Idiom.Form.equal
    ;;
  end

  type t =
    | Name of Name.t
    | Answer of Answer.t
    | Branch of Branch.t
    | Plural_lift of Binding.t
    | Field_access of Access.t
    | Chain of Chain.t
    | Mixed_notation of
        { operation : Notation.Operation.t
        ; lines : string
        }
    | Idiom of Idiom.t
    | Function_length of
        { lines : int
        ; bound : int
        ; severity : Severity.t
        }

  let severity_of = function
    | Name _ -> Severity.Error
    | Answer answer -> Answer.severity_of answer
    | Branch branch -> Branch.severity_of branch
    | Plural_lift _ | Chain _ -> Note
    | Field_access _ | Mixed_notation _ -> Warning
    | Idiom form -> (Idiom.description_of form).severity
    | Function_length { severity; _ } -> severity
  ;;

  let code_of = function
    | Name name -> Name.code_of name
    | Answer answer -> Answer.code_of answer
    | Branch branch -> Branch.code_of branch
    | Plural_lift _ -> "plural-lift"
    | Field_access _ -> "field-access"
    | Chain chain -> Chain.code_of chain
    | Mixed_notation _ -> "mixed-notation"
    | Idiom form -> (Idiom.description_of form).code
    | Function_length _ -> "function-length"
  ;;

  (* Each concept has one module, which alone knows how it holds its data. *)
  let access_message_of access =
    let owner = Access.owner_of access in
    sprintf
      "this reads a field of a record defined by %s, which ties the code to how %s holds \
       its data; could %s give a view, such as %s_of?"
      owner
      owner
      owner
      (Access.field_of access)
  ;;

  let lift_message_of singular =
    let line = Source.Site.line_of (Binding.site_of singular) in
    sprintf
      "check whether this maps %s over a collection (line %d)"
      (Binding.name_of singular)
      line
  ;;

  let notation_message_of operation lines =
    sprintf
      "this function writes %s both as an infix operator and as a let, on lines %s, so \
       keep to one notation"
      (Notation.Operation.name_of operation)
      lines
  ;;

  let length_message_of lines bound =
    sprintf "this definition has %d source lines of code, more than %d" lines bound
  ;;

  let message_of = function
    | Name name -> Name.message_of name
    | Answer answer -> Answer.message_of answer
    | Branch branch -> Branch.message_of branch
    | Plural_lift singular -> lift_message_of singular
    | Field_access access -> access_message_of access
    | Chain chain -> Chain.message_of chain
    | Mixed_notation { operation; lines } -> notation_message_of operation lines
    | Idiom form -> (Idiom.description_of form).message
    | Function_length { lines; bound; _ } -> length_message_of lines bound
  ;;
end

type t =
  { site : Source.Site.t
  ; subject : string
  ; rule : Rule.t
  }

let create ~site ~subject rule = { site; subject; rule }

let of_binding binding =
  create ~site:(Binding.site_of binding) ~subject:(Binding.name_of binding)
;;

let of_access access =
  create
    ~site:(Access.site_of access)
    ~subject:(Access.field_of access)
    (Field_access access)
;;

let of_link link =
  create ~site:(Chain.Link.site_of link) ~subject:(Chain.Link.keyword_of link)
;;

let of_idiom idiom =
  create
    ~site:(Idiom.site_of idiom)
    ~subject:(Idiom.subject_of idiom)
    (Idiom (Idiom.form_of idiom))
;;

let of_decision decision =
  create ~site:(Decision.site_of decision) ~subject:(Decision.keyword_of decision)
;;

let site_of diagnostic = diagnostic.site
let subject_of diagnostic = diagnostic.subject
let rule_of diagnostic = diagnostic.rule
let severity_of diagnostic = Rule.severity_of diagnostic.rule
let code_of diagnostic = Rule.code_of diagnostic.rule

let to_string { site; subject; rule } =
  sprintf
    "%s: %s[%s] %s: %s"
    (Source.Site.to_string site)
    (Severity.to_string (Rule.severity_of rule))
    (Rule.code_of rule)
    subject
    (Rule.message_of rule)
;;
