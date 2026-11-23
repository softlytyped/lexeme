(** A name where a program binds it: its spelling and position, its role, and what its
    type says about it. *)

open! Core

module Codomain : sig
  (** What a function gives once it has all of its arguments. *)
  type t =
    | Boolean
    | Optional_boolean
    | Fallible_boolean
    | Unit of string
    | Other of string
    | Unknown
end

module Role : sig
  type t =
    | Variable
    | Function of Codomain.t Lazy.t
end

type t = private
  { name : string
  ; role : Role.t
  ; site : Source.Site.t
  ; comment : string option (** a comment that follows the name on its line *)
  ; lines : int option (** the lines of code in a function's definition *)
  }

(** Returns the binding of [name] at [offset], and counts the lines of code in its
    [definition] when it binds a function. Returns [None] where the source does not spell
    [name] at [offset], as for a name that the compiler generated. *)
val find
  :  Source.t
  -> offset:int
  -> ?definition:Source.Extent.t
  -> name:string
  -> Role.t
  -> t option
