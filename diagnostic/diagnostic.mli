(** A diagnostic is a finding about something that a program writes, and about how it
    reads. *)

open! Core

module Severity : sig
  (** A note asks a question, a warning asks for a change, and an error fails the run. *)
  type t =
    | Note
    | Warning
    | Error
  [@@deriving equal]
end

module Rule : sig
  module Name : sig
    (** A rule of names says which phrase a name should be. *)
    type t =
      | Single_letter
      | Variable (** [Variable] asks for a noun phrase. *)
      | Function
      (** [Function] asks for a verb phrase, a predicate, or a noun phrase with a
          preposition. *)
      | Predicate
      (** [Predicate] asks for a predicate, as a function or a field that gives a Boolean
          needs. *)
      | Command
      (** [Command] asks for a verb phrase, as a function that gives nothing needs. *)
      | Field (** [Field] asks for a noun phrase. *)
      | Type (** [Type] asks for a noun phrase. *)
      | Constructor
      (** [Constructor] asks for a noun phrase, since a constructor names data. *)
  end

  module Answer : sig
    (** A rule of answers says what a predicate gives, when that is not a plain Boolean. *)
    type t =
      | Optional_boolean
      | Fallible_boolean
      | Wrong of string
      | Unknown
  end

  module Branch : sig
    (** A rule of branches says how the body of a function branches, when that could read
        better. *)
    type t =
      | Boolean_match
      (** [Boolean_match] is a pattern match on one Boolean, whose cases a variant type
          might name. *)
      | Boolean_tuple of int
      (** [Boolean_tuple] is a pattern match on several Booleans at once. *)
      | Lone_boolean_match
      (** [Lone_boolean_match] is the only Boolean condition of a function's body, written
          as a pattern match. *)
      | Several_conditionals of string
      (** [Several_conditionals lines] gives the lines of a body's if-then-else
          expressions. *)
      | Late_guard (** [Late_guard] is a guard clause that comes after unrelated work. *)
      | Nested_boolean
      (** [Nested_boolean] is a pattern match on one Boolean within another Boolean
          condition. *)
  end

  module Chain : sig
    (** A rule of chains says how a chain of local definitions mixes its styles. *)
    type t =
      | Let_pipeline (** [Let_pipeline] is a let that binds the result of a pipeline. *)
      | Late_let
      (** [Late_let] is a plain let after a bind whose result it does not read. *)
      | Lone_bind
      (** [Lone_bind] is the only bind of its chain, with plain lets on both sides of it. *)
  end

  type t =
    | Name of Name.t
    | Answer of Answer.t
    | Branch of Branch.t
    | Plural_lift of Binding.t
    (** [Plural_lift singular] gives the singular that a plural may map over a collection. *)
    | Field_access of Access.t
    (** [Field_access access] reads a field of a record defined by another compilation
        unit. *)
    | Chain of Chain.t
    | Mixed_notation of
        { operation : Notation.Operation.t
        ; lines : string
        }
    (** [Mixed_notation] is a function that writes one of a monad's operations both as an
        infix operator and as a let, on the given lines. *)
    | Function_length of
        { lines : int
        ; bound : int
        ; severity : Severity.t
        }

  (** [code_of rule] returns a rule's code, such as noun-stack. *)
  val code_of : t -> string

  (** [message_of rule] returns what a finding of a rule says. *)
  val message_of : t -> string
end

(** A finding of a rule concerns a subject written at a site, such as a binding and its
    name. *)
type t

val create : site:Source.Site.t -> subject:string -> Rule.t -> t
val site_of : t -> Source.Site.t
val subject_of : t -> string
val rule_of : t -> Rule.t

(** [of_access access] returns the warning on an access to another compilation unit's
    record. *)
val of_access : Access.t -> t

val of_binding : Binding.t -> Rule.t -> t
val of_decision : Decision.t -> Rule.t -> t
val of_link : Chain.Link.t -> Rule.t -> t
val severity_of : t -> Severity.t

(** [code_of diagnostic] returns the code of the diagnostic's rule, such as
    [variable-name]. *)
val code_of : t -> string

(** [to_string diagnostic] returns the diagnostic in the form
    [path:line:column: severity[code] subject: message]. *)
val to_string : t -> string
