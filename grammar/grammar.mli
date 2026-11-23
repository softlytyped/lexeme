(** The English phrases that can name a binding, read over the words of a name. *)

open! Core

type t

val of_lexicon : Lexicon.t -> t

module Name : sig
  type t

  (** Reads the words of a spelling, which underscores separate, past any leading
      underscores and trailing primes. *)
  val of_spelling : string -> t

  (** Returns whether the first word is an auxiliary verb, such as is, has or does, even
      when what follows it is malformed. *)
  val has_auxiliary : t -> bool

  (** Returns whether the name is a single letter. *)
  val is_letter : t -> bool
end

module Reading : sig
  type t =
    | Noun_phrase (** [matrix], [register_bits], [start_of_line] *)
    | Verb_phrase (** [apply], [fully_contract], [test_and_set] *)
    | Predicate (** [is_zero], [has_entries], [does_accept] *)
    | Nominal of string list
    (** [entries_of], [entry_at_index], [of_int]: a function named by what it gives. The
        list holds the singular spellings of a plural subject. *)
  [@@deriving equal, sexp_of]
end

(** Returns every way that the grammar reads a name. *)
val readings_of : t -> Name.t -> Reading.t list
