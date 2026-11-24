(** A use of a monad's bind or map, written in one of two forms: as an infix operator,
    such as [>>=], or as a [let], such as [let%bind]. *)

open! Core

module Operation : sig
  type t =
    | Bind (** [Bind] is [>>=], [let%bind] or [let*]. *)
    | Map (** [Map] is [>>|], [let%map] or [let+]. *)
  [@@deriving enumerate, equal]

  (** Returns the operation's name, bind or map. *)
  val name_of : t -> string
end

module Form : sig
  type t =
    | Infix_operator
    | Let_syntax
  [@@deriving equal]
end

type t

val create : site:Source.Site.t -> operation:Operation.t -> form:Form.t -> t
val site_of : t -> Source.Site.t
val operation_of : t -> Operation.t
val form_of : t -> Form.t
