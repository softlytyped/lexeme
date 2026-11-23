open! Core

module Form = struct
  type t =
    | Conditional
    | Boolean_match
    | Tuple_match of int
    | Other_match
  [@@deriving equal]
end

module Placement = struct
  type t =
    | Late_position
    | Proper_position
end

module Nesting = struct
  type t =
    | Nested_position
    | Outer_position
end

type t =
  { site : Source.Site.t
  ; keyword : string
  ; form : Form.t
  ; placement : Placement.t
  ; nesting : Nesting.t
  }
