(** A source file's text, with its lines and its comments. Offsets count bytes from zero,
    and lines and columns count from one. *)

open! Core

module Extent : sig
  (** The bytes from [start] up to but not including [stop]. *)
  type t =
    { start : int
    ; stop : int
    }
  [@@deriving compare, sexp_of]

  include Comparable.S_plain with type t := t
end

module Comment : sig
  (** A comment's extent includes its delimiters, and its body excludes them. *)
  type t =
    { extent : Extent.t
    ; body : string
    }
end

module Position : sig
  type t =
    { line : int
    ; column : int
    }
  [@@deriving compare]
end

module Site : sig
  (** Where something is written. *)
  type t =
    { path : string
    ; position : Position.t
    }
  [@@deriving compare]
end

type t

val create : path:string -> comments:Comment.t list -> string -> t
val site_at : t -> offset:int -> Site.t

(** Returns whether the text spells [spelling] at [offset]. *)
val does_spell : t -> offset:int -> string -> bool

(** Counts the lines within an extent that hold code: a character that is neither white
    space nor part of a comment. *)
val code_lines_in : t -> Extent.t -> int

(** Returns the body of the comment that opens after [offset] on the same line, past
    spaces and tabs alone. *)
val comment_after : t -> offset:int -> string option
