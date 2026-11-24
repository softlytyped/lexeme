(** One Merlin session over an OCaml source: its written syntax and comments before ppx
    expansion, and the compiler's types. *)

open! Core

type t

(** Parses the source. Fails with the parser's diagnostics when the source is malformed.
    Typing waits for the first {!shape_at}. *)
val create : path:string -> string -> t Or_error.t

val syntax_tree_of : t -> Merlin_kernel.Mreader.parsetree
val comments_of : t -> Source.Comment.t list
val extent_of : Ocaml_parsing.Location.t -> Source.Extent.t

module Shape : sig
  (** A value's type, or a function's, with what it gives. *)
  type 'codomain t =
    | Value of 'codomain
    | Function of 'codomain
end

(** Returns whether the project builds the module of a name, rather than installs it as a
    library: its compiled interface lies in one of the directories of the build path, but
    not beside the standard library, whose directory holds stdlib.cmi. *)
val is_built_here : build_path:string list -> string -> bool

(** Returns the module that defines the record of the field whose name a field access or a
    record pattern writes at an extent, where that module is another compilation unit and
    the source types well enough to say. *)
val owner_at : t -> Source.Extent.t -> string option

(** Returns the compiler's shape for the declaration at an extent, where the source types
    well enough to say. *)
val shape_at : t -> Source.Extent.t -> Binding.Codomain.t Lazy.t Shape.t option

(** Fails when typing was attempted and failed. *)
val verify : t -> unit Or_error.t
