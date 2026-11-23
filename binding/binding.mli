(** A name where a program binds it: its spelling and position, its role, and what its
    type says about it. *)

open! Core

module Codomain : sig
  (** What a function gives once it has all of its arguments. A field's codomain is that
      of its projection, the type of what it holds. *)
  type t =
    | Boolean
    | Optional_boolean
    | Fallible_boolean
    | Unit of string
    | Other_type of string
    | Unknown
end

module Role : sig
  type t =
    | Variable
    | Function of Codomain.t Lazy.t
    | Field of Codomain.t Lazy.t (** a record field that holds no function *)
    | Type
    | Constructor
    | Test (** an inline test, named by its description *)
end

type t = private
  { name : string
  ; role : Role.t
  ; site : Source.Site.t
  ; comment : string option (** a comment that follows the name on its line *)
  ; lines : int option (** the lines of code in the definition of a function or test *)
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

(** Returns the binding of an inline test whose description opens at [offset]. *)
val of_test
  :  Source.t
  -> offset:int
  -> description:string
  -> definition:Source.Extent.t
  -> t
