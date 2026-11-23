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
      | Function (** a verb phrase, a prepositional name or a predicate *)
      | Predicate (** a predicate, for a function or field that gives a Boolean *)
      | Command (** a verb phrase, for a function that gives nothing *)
      | Field (** a noun phrase *)
      | Type (** a noun phrase *)
      | Constructor (** a noun phrase or an adjective phrase *)
  end

  module Answer : sig
    (** What a predicate gives, when that is not a plain Boolean. *)
    type t =
      | Optional
      | Fallible
      | Wrong of string
      | Unknown
  end

  module Branch : sig
    (** How a function body branches, when that could read better. *)
    type t =
      | Boolean_match (** a match on one Boolean, which a variant might name *)
      | Boolean_tuple of int (** a match on several Booleans at once *)
      | Lone_boolean_match (** a function body's one Boolean decision, as a match *)
      | Several_conditionals of string (** the lines of a function body's if-else *)
      | Late_guard (** a guard after work that it does not use *)
      | Nested_boolean (** a match on one Boolean within another Boolean decision *)
  end

  type t =
    | Name of Name.t
    | Answer of Answer.t
    | Branch of Branch.t
    | Plural_lift of Binding.t (** the singular function that the plural may lift *)
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
