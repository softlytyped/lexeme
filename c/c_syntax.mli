(** The bindings of a C translation unit in the active preprocessor configuration. *)

open! Core

(** Folds declarations spelled in the source file. Include directories and macro
    definitions go to Clang. Fails when the translation unit cannot be parsed. *)
val fold
  :  Binding_syntax.Source.t
  -> directories:string list
  -> definitions:string list
  -> 'result Binding_syntax.algebra
  -> 'result Or_error.t
