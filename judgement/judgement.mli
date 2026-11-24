(** The interpretation of a program's names, decisions and scopes as diagnostics. *)

open! Core

module Limits : sig
  (** A definition with more lines of code than [note] earns a note, more than [warning] a
      warning and more than [error] an error. An inline test may have [test_allowance]
      more lines at each bound. *)
  type t =
    { note : int
    ; warning : int
    ; error : int
    ; test_allowance : int
    }
  [@@deriving of_sexp]

  val default : t
end

type t

(** Returns the algebra that judges each binding by its name, its type and its length,
    each function body by its decisions and by the notations of its monads, each chain of
    lets by how it mixes its styles, each field access by the record that it uses, each
    idiom by the plainer form that can replace it, and each scope by the plural lifts
    among its declarations. A name in [exemptions] is not judged as a name, though its
    definition still is. *)
val algebra_of
  :  Lexicon.t
  -> limits:Limits.t
  -> exemptions:String.Set.t
  -> t Syntax.algebra

(** Returns a file's diagnostics in source order. *)
val diagnostics_of : t -> Diagnostic.t list
