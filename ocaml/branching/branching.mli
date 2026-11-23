(** How an OCaml source branches. *)

open! Core
open Ocaml_parsing

(** Returns the form of a match on these cases, by the Booleans its patterns name. *)
val form_of_cases : Parsetree.case list -> Decision.Form.t

(** Returns the extents of the late guards in each function body: the conditionals that a
    body's chain of [let]s reaches after work whose names neither their condition nor one
    of their branches uses. *)
val late_guards_in : Merlin_kernel.Mreader.parsetree -> Source.Extent.Set.t
