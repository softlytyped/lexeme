(** A finding about one binding, and how it reads. *)

open! Core

module Severity : sig
  (** A note asks a question, a warning asks for a change, and an error fails the run. *)
  type t =
    | Note
    | Warning
    | Error
  [@@deriving equal]
end

module Limit : sig
  (** The bounds on a function's lines of code, past which it earns a note, a warning and
      an error. *)
  type t =
    | Soft
    | Hard
    | Reject
end

module Rule : sig
  module Name : sig
    (** The phrase that a name should be, and is not. *)
    type t =
      | Single_letter
      | Variable (** a noun phrase *)
      | Function (** a verb phrase, a prepositional name or a predicate *)
      | Predicate (** a predicate, for a function that gives a Boolean *)
      | Command (** a verb phrase, for a function that gives nothing *)
  end

  module Answer : sig
    (** What a predicate gives, when that is not a plain Boolean. *)
    type t =
      | Optional
      | Fallible
      | Wrong of string
      | Unknown
  end

  type t =
    | Name of Name.t
    | Answer of Answer.t
    | Plural_lift of Binding.t (** the singular function that the plural may lift *)
    | Function_length of
        { lines : int
        ; limit : Limit.t
        ; bound : int
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
val severity_of : t -> Severity.t

(** Returns the diagnostic as [path:line:column: severity[code] subject: message]. *)
val to_string : t -> string
