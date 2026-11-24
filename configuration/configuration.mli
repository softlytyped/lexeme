(** A project's settings, read from its [.lexeme] file. Every field is optional. *)

open! Core

(** A project's settings: the vocabulary that it adds to English, the limits on the length
    of a definition, the names not judged as names ([main] and [equal] by default), and
    the include directories and the preprocessor macros of its C sources. *)
type t

val vocabulary_of : t -> Lexicon.Vocabulary.t
val function_lines_of : t -> Judgement.Limits.t
val exemptions_of : t -> string list
val directories_of : t -> string list
val definitions_of : t -> string list

(** [extend configuration ~directories ~definitions] returns the settings with more
    include directories and more macros, such as those that a command line gives. *)
val extend : t -> directories:string list -> definitions:string list -> t

(** Reads the settings at [path], or else in the nearest [.lexeme] in the working
    directory or one of its ancestors, and gives the defaults when there is none. Include
    directories are relative to the file. Fails when the file cannot be read or is
    malformed. *)
val load : ?path:string -> unit -> t Or_error.t
