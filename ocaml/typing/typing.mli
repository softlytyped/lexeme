(** The role of a written OCaml binding. The compiler's shape wins, and a written
    annotation answers where a source that does not type leaves a codomain unknown. *)

open! Core
open Ocaml_parsing

(** A value binding binds a variable or a function. *)
val role_of_definition
  :  Session.t
  -> Parsetree.value_binding
  -> string Location.loc
  -> Binding.Role.t

(** A value description, [val name : annotation], specifies a variable or a function. *)
val role_of_description
  :  Session.t
  -> Parsetree.core_type
  -> string Location.loc
  -> Binding.Role.t

(** A record label names a field or, where it holds one, a function. *)
val role_of_label
  :  Session.t
  -> Parsetree.core_type
  -> string Location.loc
  -> Binding.Role.t

(** A method is a function of its object. *)
val role_of_method
  :  ?annotation:Parsetree.core_type
  -> ?extent:Location.t
  -> Session.t
  -> string Location.loc
  -> Binding.Role.t
