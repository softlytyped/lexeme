(** The grammar of a value name, read in the role of its binding. *)

open! Core

(** The grammatical reading of a name. Its singulars replace only the head of its subject
    noun phrase, keeping modifiers, prepositions and complements. *)
type t

val singulars_of : t -> string list

(** Returns a reading when the words form a name of the given role. *)
val read : Lexicon.t -> Binding_syntax.role -> string -> t option

(** Tests for one letter beneath OCaml's unused-variable prefix and prime suffix. *)
val is_letter : string -> bool

(** Tests whether a name opens with a predicate auxiliary. *)
val is_predicate : string -> bool
