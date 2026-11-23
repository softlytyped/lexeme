(** The role of a written OCaml binding. The compiler's role wins, and a written
    annotation answers where a source that does not type leaves a codomain unknown. *)

open! Core
open Ocaml_parsing

val role_of_definition
  :  Session.t
  -> Parsetree.value_binding
  -> string Location.loc
  -> Binding.Role.t

val role_of_type
  :  Session.t
  -> Parsetree.core_type
  -> string Location.loc
  -> Binding.Role.t

val role_of_method
  :  ?annotation:Parsetree.core_type
  -> ?extent:Location.t
  -> Session.t
  -> string Location.loc
  -> Binding.Role.t
