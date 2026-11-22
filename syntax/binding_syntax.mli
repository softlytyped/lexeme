(** The part of a language's syntax that introduces names. Language readers fold their
    syntax directly into an algebra. Each scope receives the interpretations of its own
    declarations and of its enclosed scopes, in source order. *)

open! Core

module Source : sig
  type t

  val source_of_string : path:string -> string -> t
  val path_of : t -> string
  val text_of : t -> string
end

type result =
  | Boolean
  | Optional_boolean
  | Fallible_boolean
  | Other of string
  | Unknown
[@@deriving equal, sexp_of]

type role =
  | Variable
  | Function of result Lazy.t
[@@deriving sexp_of]

type binding = private
  { name : string
  ; role : role
  ; path : string
  ; line : int
  ; column : int
  ; letter_exception : string option
  }
[@@deriving sexp_of]

(** The binding at a byte offset in the source. Returns [None] for compiler-generated
    names with no corresponding spelling at that offset. Lines and columns start at one. *)
val binding_at : Source.t -> offset:int -> name:string -> role -> binding option

type 'result algebra =
  { bind : binding -> 'result
  ; enclose : 'result list -> 'result
  }
