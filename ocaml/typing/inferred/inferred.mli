(** The written syntax and the compiler types of one OCaml source. *)

open! Core

type t

(** Parses the source before ppx expansion. Inference shares the same compiler session and
    runs on the first request for a binding's role. *)
val read : Binding_syntax.Source.t -> (Merlin_kernel.Mreader.parsetree * t) Or_error.t

val role_at : t -> start:int -> stop:int -> Binding_syntax.role option
val verify : t -> unit Or_error.t
