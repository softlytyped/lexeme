(** The bindings of an OCaml implementation or interface, before ppx expansion. *)

open! Core

(** Reads the program of a source's bindings. Fails with the parser's diagnostics when the
    source is malformed, and when the compiler's types cannot be read. *)
val read : path:string -> string -> unit Syntax.t Or_error.t
