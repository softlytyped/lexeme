(** The syntax of a source: the free monad over five operations, which all answer [unit].
    [declare] binds a name in the current scope, [decide] branches, and [access] reads a
    field of a record that another compilation unit defines. [enclose] and [define] are
    scoped: [enclose] runs a computation in a lexical block of its own, and [define]
    declares a function and runs its body. A reader writes a source file as a program, and
    a check is an algebra for the program's fold. *)

open! Core

type 'a t

include Monad.S with type 'a t := 'a t

val declare : Binding.t -> unit t
val decide : Decision.t -> unit t
val access : Access.t -> unit t
val enclose : unit t -> unit t
val define : Binding.t -> unit t -> unit t

(** A direct-style traversal's view of the program it performs. *)
module Recorder : sig
  type t

  val declare : t -> Binding.t -> unit
  val decide : t -> Decision.t -> unit
  val access : t -> Access.t -> unit

  (** Runs a traversal of one scope. *)
  val enclose : t -> (unit -> unit) -> unit

  (** Runs a traversal of a function's body. *)
  val define : t -> Binding.t -> (unit -> unit) -> unit
end

(** Returns the program that a direct-style traversal performs through its recorder. *)
val record : (Recorder.t -> unit) -> unit t

type 'result algebra =
  { declare : Binding.t -> 'result
  ; decide : Decision.t -> 'result
  ; access : Access.t -> 'result
  ; enclose : 'result list -> 'result
  ; define : Binding.t -> 'result list -> 'result
  }

(** Interprets a file's program: each operation by its part of the algebra, with each
    scoped operation given its body's interpretations in source order. A scope that holds
    nothing is left out, and the file is the outermost scope. *)
val fold : 'result algebra -> unit t -> 'result
