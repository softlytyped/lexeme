(** An access is a program's use of a field of a record defined by another compilation
    unit, by the field's name in an expression or in a pattern. *)

open! Core

type t

(** [create ~site ~field ~owner] returns an access to a field of a record whose module is
    the owner, such as Source.Site. *)
val create : site:Source.Site.t -> field:string -> owner:string -> t

val site_of : t -> Source.Site.t
val field_of : t -> string
val owner_of : t -> string
