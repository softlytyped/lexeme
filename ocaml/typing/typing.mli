(** The OCaml types at the value bindings of one source text. *)

open! Core

type t

(** The compiler's outline is read on first demand and shared across bindings. *)
val types_of : Binding_syntax.Source.t -> t

val is_function : Parsetree.core_type -> bool
val annotation_at : t -> string Location.loc -> Parsetree.core_type option

(** Reports a failed compiler query if the fold needed the outline. *)
val verify : t -> unit Or_error.t

(** Classifies the final result after all arguments. Uses a source annotation when
    available and asks Merlin to expand type aliases at the binding when needed. *)
val result_at
  :  ?extent:Location.t
  -> t
  -> string Location.loc
  -> annotation:Parsetree.core_type option
  -> Binding_syntax.result
