open! Core

(** Runs a program with its argument vector and reads its standard output to completion.
    Its diagnostics go to standard error. Fails when the program or its output fails. *)
val read
  :  ?input:string
  -> ?environment:(string * string option) list
  -> string
  -> string list
  -> string Or_error.t
