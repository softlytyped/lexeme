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

let create ~site ~keyword ~form ~placement ~nesting =
  { site; keyword; form; placement; nesting }
;;

let site_of decision = decision.site
let keyword_of decision = decision.keyword
let form_of decision = decision.form
let placement_of decision = decision.placement
let nesting_of decision = decision.nesting
