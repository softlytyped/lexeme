(** A point where a function body branches. *)

open! Core

module Form : sig
  type t =
    | Conditional (** [if], with or without [else] *)
    | Boolean_match (** a match on one Boolean *)
    | Tuple_match of int
    (** a match on a tuple, with the count of its Boolean components *)
    | Other_match
  [@@deriving equal]
end

module Placement : sig
  type t =
    | Late (** a guard after work whose names neither its condition nor a branch uses *)
    | Settled
end

module Nesting : sig
  type t =
    | Nested (** within a branch of a Boolean decision of the same function body *)
    | Outermost
end

type t =
  { site : Source.Site.t
  ; keyword : string (** such as [if] or [match], to name the decision *)
  ; form : Form.t
  ; placement : Placement.t
  ; nesting : Nesting.t
  }
