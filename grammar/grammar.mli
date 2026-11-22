(** The English phrases that can name a binding. *)

open! Core

type t

val grammar_of : Lexicon.t -> t

type name

val read : string -> name
val is_noun : t -> name -> bool

(** Returns the possible singular spellings of a function's subject. A verb phrase, a
    predicate and a prepositional name without a subject have no singulars. *)
val function_of : t -> name -> string list option

(** Whether the initial word introduces a predicate, even if its complement is malformed. *)
val is_predicate : name -> bool

(** Tests for a single letter beneath an unused-name prefix and prime suffix. *)
val is_letter : string -> bool
