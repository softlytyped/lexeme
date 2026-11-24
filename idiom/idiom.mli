(** An idiom is a small form of an expression that a plainer form can replace, such as
    [if ready then true else false] in place of [ready]. *)

open! Core

module Form : sig
  type t =
    | One_stage_pipeline
    (** [One_stage_pipeline] is a pipeline of one stage, [x |> f], in place of [f x]. *)
    | Boolean_branches
    (** [Boolean_branches] is an if-then-else whose branches are [true] and [false], in
        place of its condition or the negation of it. *)
    | Eta_expansion
    (** [Eta_expansion] is an anonymous function that applies a function to its parameter,
        [fun x -> f x], in place of [f]. *)
    | One_case_function
    (** [One_case_function] is a [function] with a single case, in place of [fun]. *)
    | Discard_without_type
    (** [Discard_without_type] is [ignore] applied to a value without a type annotation. *)
    | Length_comparison
    (** [Length_comparison] compares the length of a list, a string or an array with zero,
        in place of [is_empty]. *)
    | Boolean_parameter
    (** [Boolean_parameter] is a parameter of type [bool] in a named function. *)
    | Nested_anonymous_function
    (** [Nested_anonymous_function] is an anonymous function within two others. *)
  [@@deriving enumerate, equal, sexp_of]
end

type t

(** [create ~site ~subject form] returns an idiom of a form, written at a site and named
    by its subject, such as the operator [|>] or a parameter's name. *)
val create : site:Source.Site.t -> subject:string -> Form.t -> t

val site_of : t -> Source.Site.t
val subject_of : t -> string
val form_of : t -> Form.t
