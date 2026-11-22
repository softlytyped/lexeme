(** The OCaml parser's binding sites, before ppx expansion. *)

open! Core

(** Folds an implementation or interface into the algebra. Returns the parser's diagnostic
    when the input is malformed. *)
val fold : Binding_syntax.Source.t -> 'result Binding_syntax.algebra -> 'result Or_error.t
