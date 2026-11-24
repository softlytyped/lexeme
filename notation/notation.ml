open! Core

module Operation = struct
  type t =
    | Bind
    | Map
  [@@deriving enumerate, equal]

  let name_of = function
    | Bind -> "bind"
    | Map -> "map"
  ;;
end

module Form = struct
  type t =
    | Infix_operator
    | Let_syntax
  [@@deriving equal]
end

type t =
  { site : Source.Site.t
  ; operation : Operation.t
  ; form : Form.t
  }

let create ~site ~operation ~form = { site; operation; form }
let site_of notation = notation.site
let operation_of notation = notation.operation
let form_of notation = notation.form
