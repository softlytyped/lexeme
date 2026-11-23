(** The parts of speech that a word of a name can take. A word may have several. *)

open! Core

type t

(** Princeton WordNet 3.0, restricted to single words and the inflections of nouns. *)
val english : t Lazy.t

module Vocabulary : sig
  (** The readings that a project adds to English. A plural may have several singulars. *)
  type t =
    { nouns : string list
    ; verbs : string list
    ; adjectives : string list
    ; adverbs : string list
    ; plurals : (string * string) list
    }
  [@@deriving of_sexp]

  val empty : t
end

(** Adds a project's readings. Fails when an entry is not one lowercase word. *)
val extend : t -> Vocabulary.t -> t Or_error.t

val is_noun : t -> string -> bool
val is_verb : t -> string -> bool

(** Returns whether a word is an adjective, or a participle of an attested verb. *)
val is_adjective : t -> string -> bool

val is_adverb : t -> string -> bool

(** Returns the attested singular nouns whose plural is the given word. *)
val singulars_of : t -> string -> string list
