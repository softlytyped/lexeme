(** The interpretation of a program's bindings and scopes as diagnostics. *)

open! Core

module Limits : sig
  (** A function definition with more than [soft] lines of code earns a note, more than
      [hard] a warning and more than [reject] an error. *)
  type t =
    { soft : int
    ; hard : int
    ; reject : int
    }
  [@@deriving of_sexp]

  val default : t
end

type t

(** Returns the algebra that judges each binding by its name, its type and its length, and
    each scope by the plural lifts among its declarations. A name in [exemptions] is not
    judged as a name, though its definition's length still is. *)
val algebra_of
  :  Lexicon.t
  -> limits:Limits.t
  -> exemptions:String.Set.t
  -> t Syntax.algebra

(** Returns a file's diagnostics in source order. *)
val diagnostics_of : t -> Diagnostic.t list
