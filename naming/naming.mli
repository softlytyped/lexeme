(** An interpretation of bindings and scopes as naming diagnostics. *)

open! Core

type rule =
  | Single_letter
  | Variable_name
  | Function_name
  | Plural_lift of Binding_syntax.binding
  | Optional_result
  | Fallible_result
  | Wrong_result of string
  | Unknown_result

type diagnostic = private
  { binding : Binding_syntax.binding
  ; rule : rule
  }

val is_error : diagnostic -> bool
val format : diagnostic -> string

type t

(** Interprets a scope after all of its immediate bindings are known. *)
val algebra_of : Lexicon.t -> t Binding_syntax.algebra

(** Returns the diagnostics of an interpreted file in source order. *)
val diagnostics_of : t -> diagnostic list
