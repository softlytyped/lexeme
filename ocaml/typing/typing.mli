(** The role of a written OCaml binding. An explicit annotation supplies the judgement
    when a partially typed source has no inferred codomain. *)

open! Core
open Ocaml_parsing

val role_of_definition
  :  Inferred.t
  -> Parsetree.value_binding
  -> string Location.loc
  -> Binding_syntax.role

val role_of_type
  :  Inferred.t
  -> Parsetree.core_type
  -> string Location.loc
  -> Binding_syntax.role

val method_at
  :  ?annotation:Parsetree.core_type
  -> ?extent:Location.t
  -> Inferred.t
  -> string Location.loc
  -> Binding_syntax.role
