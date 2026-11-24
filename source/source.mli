(** A source holds a file's text, with its lines and its comments. And offsets are counted
    in bytes from zero, while lines and columns are counted from one. *)

open! Core

module Extent : sig
  (** An extent holds the bytes from a start up to, but not including, a stop. *)
  type t [@@deriving compare, sexp_of]

  include Comparable.S_plain with type t := t

  val create : start:int -> stop:int -> t
  val start_of : t -> int
  val stop_of : t -> int
end

module Comment : sig
  (** The extent of a comment includes its delimiters, but its body leaves them out. *)
  type t

  val create : extent:Extent.t -> body:string -> t
  val extent_of : t -> Extent.t
  val body_of : t -> string
end

module Site : sig
  (** A site tells where something is written: a file's path, and a line and a column in
      it. *)
  type t [@@deriving compare]

  val path_of : t -> string
  val line_of : t -> int
  val column_of : t -> int

  (** [to_string site] returns the path, the line and the column, separated by colons. *)
  val to_string : t -> string
end

type t

val create : path:string -> comments:Comment.t list -> string -> t
val site_at : t -> offset:int -> Site.t

(** [text_in source extent] returns the text within an extent, or none when the extent
    does not lie within the text. *)
val text_in : t -> Extent.t -> string option

(** [does_spell source ~offset spelling] answers whether the text spells a word at an
    offset. *)
val does_spell : t -> offset:int -> string -> bool

(** [code_lines_in source extent] counts the lines within an extent that hold code, which
    means a character that is neither white space nor part of a comment. *)
val code_lines_in : t -> Extent.t -> int

(** [comment_after source ~offset] returns the body of the comment that opens after an
    offset on the same line, past spaces and tabs alone. *)
val comment_after : t -> offset:int -> string option
