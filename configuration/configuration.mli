(** A project's settings, read from its [.lexeme] file. Every field is optional. *)

open! Core

type t =
  { vocabulary : Lexicon.Vocabulary.t
  ; function_lines : Judgement.Limits.t
  ; exemptions : string list (** names not judged as names, [main] and [equal] by default *)
  ; directories : string list (** C include directories *)
  ; definitions : string list (** C preprocessor macros *)
  }

(** Reads the settings at [path], or else in the nearest [.lexeme] in the working
    directory or one of its ancestors, and gives the defaults when there is none. Include
    directories are relative to the file. Fails when the file cannot be read or is
    malformed. *)
val load : ?path:string -> unit -> t Or_error.t
