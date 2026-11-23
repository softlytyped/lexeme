(** One Merlin session over an OCaml source: its written syntax and comments before ppx
    expansion, and the compiler's types. *)

open! Core

type t

(** Parses the source. Fails with the parser's diagnostics when the source is malformed.
    Typing waits for the first {!role_at}. *)
val create : path:string -> string -> t Or_error.t

val parsetree_of : t -> Merlin_kernel.Mreader.parsetree
val comments_of : t -> Source.Comment.t list
val extent_of : Ocaml_parsing.Location.t -> Source.Extent.t

(** Returns the compiler's role for the declaration at an extent, when the source types. *)
val role_at : t -> Source.Extent.t -> Binding.Role.t option

(** Fails when typing was attempted and failed. *)
val verify : t -> unit Or_error.t
