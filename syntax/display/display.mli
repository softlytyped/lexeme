(** Programs as S-expressions, for tests of the readers. *)

open! Core

(** The algebra that spells each declaration as its name and role, each conditional by its
    keyword, each block as a list and each function body after its function. *)
val algebra : Sexp.t Syntax.algebra
