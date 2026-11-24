(** A decision is a conditional in a function body. *)

open! Core

module Form : sig
  type t =
    | Conditional (** [Conditional] is an [if], with or without an [else]. *)
    | Boolean_match (** [Boolean_match] is a pattern match on a single Boolean value. *)
    | Tuple_match of int
    (** [Tuple_match count] is a pattern match on a tuple, with the number of its Boolean
        components. *)
    | Other_match
  [@@deriving equal]
end

module Placement : sig
  type t =
    | Late_position
    (** [Late_position] marks a guard clause that comes after unrelated work: neither its
        condition nor its branches use the work before it. *)
    | Proper_position
end

module Nesting : sig
  type t =
    | Nested_position
    (** [Nested_position] marks a decision within a branch of a Boolean condition of the
        same function body. *)
    | Outer_position
end

type t

(** [create ~site ~keyword ~form ~placement ~nesting] returns a decision, named by its
    keyword, such as [if] or [match]. *)
val create
  :  site:Source.Site.t
  -> keyword:string
  -> form:Form.t
  -> placement:Placement.t
  -> nesting:Nesting.t
  -> t

val site_of : t -> Source.Site.t
val keyword_of : t -> string
val form_of : t -> Form.t
val placement_of : t -> Placement.t
val nesting_of : t -> Nesting.t
