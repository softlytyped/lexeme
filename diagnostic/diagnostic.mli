(** A finding about something a program writes, and how it reads. *)

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
    (** The phrase that a name should be, and is not. *)
    type t =
      | Single_letter
      | Variable (** a noun phrase *)
      | Function (** a verb phrase, a predicate or a noun phrase with a preposition *)
      | Predicate (** a predicate, for a function or field that gives a Boolean *)
      | Command (** a verb phrase, for a function that gives nothing *)
      | Field (** a noun phrase *)
      | Type (** a noun phrase *)
      | Constructor (** a noun phrase, since a constructor names data *)
  end

  module Answer : sig
    (** What a predicate gives, when that is not a plain Boolean. *)
    type t =
      | Optional_boolean
      | Fallible_boolean
      | Wrong of string
      | Unknown
  end

  module Branch : sig
    (** How a function body branches, when that could read better. *)
    type t =
      | Boolean_match (** a match on one Boolean, which a variant type might name *)
      | Boolean_tuple of int (** a match on several Booleans at once *)
      | Lone_boolean_match (** a function body's only Boolean condition, as a match *)
      | Several_conditionals of string
      (** the lines of a body's if-then-else expressions *)
      | Late_guard (** a guard clause after work that it does not use *)
      | Nested_boolean (** a match on one Boolean nested in another Boolean condition *)
  end

  type t =
    | Name of Name.t
    | Answer of Answer.t
    | Branch of Branch.t
    | Plural_lift of Binding.t
    (** the singular that the plural may map over a collection *)
    | Function_length of
        { lines : int
        ; bound : int
        ; severity : Severity.t
        }
end

(** A finding about a subject written at a site, such as a binding and its name. *)
type t = private
  { site : Source.Site.t
  ; subject : string
  ; rule : Rule.t
  }

val create : site:Source.Site.t -> subject:string -> Rule.t -> t
val of_binding : Binding.t -> Rule.t -> t
val of_decision : Decision.t -> Rule.t -> t
val severity_of : t -> Severity.t

(** Returns the name of the rule, such as [variable-name]. *)
val code_of : t -> string

(** Returns the diagnostic as [path:line:column: severity[code] subject: message]. *)
val to_string : t -> string
