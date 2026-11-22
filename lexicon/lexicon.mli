(** The parts of speech of an identifier word. A word may have several readings. *)

open! Core

type t

(** WordNet 3.0, restricted to single words and their noun inflections. *)
val english : t Lazy.t

(** The entries a project adds to English. A plural may have several singulars. *)
type vocabulary =
  { nouns : string list [@sexp.list]
  ; verbs : string list [@sexp.list]
  ; adjectives : string list [@sexp.list]
  ; adverbs : string list [@sexp.list]
  ; plurals : (string * string) list [@sexp.list]
  }
[@@deriving of_sexp]

(** Adds readings. Fails when an entry is not one lowercase identifier word. *)
val extend : t -> vocabulary -> t Or_error.t

val is_noun : t -> string -> bool
val is_verb : t -> string -> bool
val is_adjective : t -> string -> bool
val is_adverb : t -> string -> bool

(** The attested singular nouns whose plural can be the given word. *)
val singulars_of : t -> string -> string list
