(** Child programs. *)

open! Core

(** Runs a program with its arguments and gives its standard output. The program reads
    [input], or nothing, and its diagnostics go to standard error. Fails when the program
    cannot start or exits unsuccessfully. *)
val run : ?input:string -> string -> string list -> string Or_error.t
