(** The syntax of names: the free monad over two operations. [declare] binds a name in the
    current scope. [enclose] is scoped: it runs a computation in a lexical scope of its
    own and answers with the computation's answer. A reader writes a source file as a
    program, and a check is an algebra for the program's fold. *)

open! Core

type 'a t

include Monad.S with type 'a t := 'a t

val declare : Binding.t -> unit t
val enclose : 'a t -> 'a t

(** A direct-style traversal's view of the program it performs. *)
module Recorder : sig
  type t

  val declare : t -> Binding.t -> unit

  (** Runs a traversal of one scope. *)
  val enclose : t -> (unit -> unit) -> unit
end

(** Returns the program that a direct-style traversal performs through its recorder. *)
val record : (Recorder.t -> unit) -> unit t

type 'result algebra =
  { declare : Binding.t -> 'result
  ; enclose : 'result list -> 'result
  }

(** Interprets a file's program: each declaration by [declare], and each scope that
    declares anything by [enclose] of its children's interpretations in source order. The
    file is the outermost scope. *)
val fold : 'result algebra -> unit t -> 'result
