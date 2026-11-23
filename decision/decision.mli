(** A conditional in a function body. *)

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
    | Late_position
    (** a guard clause after work that neither its condition nor a branch uses *)
    | Proper_position
end

module Nesting : sig
  type t =
    | Nested_position
    (** within a branch of a Boolean condition of the same function body *)
    | Outer_position
end

type t =
  { site : Source.Site.t
  ; keyword : string (** such as [if] or [match], to name the decision *)
  ; form : Form.t
  ; placement : Placement.t
  ; nesting : Nesting.t
  }
