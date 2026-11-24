(** How an OCaml source branches. *)

open! Core
open Ocaml_parsing

(** Returns the form of a match on these cases, by the Booleans its patterns name. *)
val form_of_cases : Parsetree.case list -> Decision.Form.t

(** [identifiers_in expression] returns the names of local values that an expression uses. *)
val identifiers_in : Parsetree.expression -> String.Set.t

(** Returns the extents of the guard clauses that come late in each function body: those
    that the body's chain of [let]s reaches after work which neither their condition nor
    one of their branches uses. *)
val late_guards_in : Merlin_kernel.Mreader.parsetree -> Source.Extent.Set.t

(** Returns the extents of the matches on one Boolean that lie within a branch of another
    Boolean decision of the same function body. *)
val nested_booleans_in : Merlin_kernel.Mreader.parsetree -> Source.Extent.Set.t
