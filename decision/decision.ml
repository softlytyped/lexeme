open! Core

module Form = struct
  type t =
    | If
    | Boolean_match
    | Tuple_match of int
    | Other_match
  [@@deriving equal]
end

module Placement = struct
  type t =
    | Late
    | Settled
end

module Nesting = struct
  type t =
    | Nested
    | Outermost
end

type t =
  { site : Source.Site.t
  ; keyword : string
  ; form : Form.t
  ; placement : Placement.t
  ; nesting : Nesting.t
  }
