(** A chain of local definitions, such as [let ... in let ... in body] in OCaml. Each link
    binds names to the value of an expression, either directly, as [let] does, or through
    a monad, as [let%bind] and [let%map] do. *)

open! Core

module Link : sig
  module Form : sig
    type t =
      | Plain_binding
      (** [Plain_binding] binds the value of its expression, as [let] does. *)
      | Monad_binding
      (** [Monad_binding] binds the value of a computation, as [let%bind], [let%map] and
          [let*] do. *)
    [@@deriving equal]
  end

  module Definition : sig
    type t =
      | Pipeline
      (** [Pipeline] is an expression whose outermost operator is [|>], such as
          [words |> List.map ~f:spelling_of]. *)
      | Other_expression
  end

  type t

  (** [create ~site ~keyword ~form ~definition ~names ~reads] returns a link, named by its
      keyword, such as [let] or [let%bind], that binds [names] to a definition whose
      expression reads the names [reads]. *)
  val create
    :  site:Source.Site.t
    -> keyword:string
    -> form:Form.t
    -> definition:Definition.t
    -> names:string list
    -> reads:string list
    -> t

  val site_of : t -> Source.Site.t
  val keyword_of : t -> string
  val form_of : t -> Form.t
  val definition_of : t -> Definition.t
  val names_of : t -> string list
  val reads_of : t -> string list
end

type t

(** Returns the chain of these links, in source order. *)
val create : Link.t list -> t

val links_of : t -> Link.t list
