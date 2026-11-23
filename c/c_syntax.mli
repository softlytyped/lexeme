(** The bindings of a C translation unit, in the active preprocessor configuration. *)

open! Core

(** Reads the program of the declarations spelled in a C source. Clang parses the text,
    with the source's directory, the include [directories] and the macro [definitions].
    Declarations from an included file are read when that file is. Fails when Clang cannot
    parse the translation unit. *)
val read
  :  path:string
  -> directories:string list
  -> definitions:string list
  -> string
  -> unit Syntax.t Or_error.t
