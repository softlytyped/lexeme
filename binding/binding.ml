open! Core

module Codomain = struct
  type t =
    | Boolean
    | Optional_boolean
    | Fallible_boolean
    | Unit of string
    | Other_type of string
    | Unknown
end

module Role = struct
  type t =
    | Variable
    | Function of Codomain.t Lazy.t
    | Field of Codomain.t Lazy.t
    | Type
    | Constructor
    | Test
end

type t =
  { name : string
  ; role : Role.t
  ; site : Source.Site.t
  ; comment : string option
  ; lines : int option
  }

let definition_lines_of source role definition =
  match role with
  | Role.Function _ | Test -> Option.map definition ~f:(Source.code_lines_in source)
  | Variable | Field _ | Type | Constructor -> None
;;

let name_of binding = binding.name
let role_of binding = binding.role
let site_of binding = binding.site
let comment_of binding = binding.comment
let lines_of binding = binding.lines

let find source ~offset ?definition ~name role =
  let%map.Option () = Option.some_if (Source.does_spell source ~offset name) () in
  { name
  ; role
  ; site = Source.site_at source ~offset
  ; comment = Source.comment_after source ~offset:(offset + String.length name)
  ; lines = definition_lines_of source role definition
  }
;;

let of_test source ~offset ~description ~definition =
  { name = sprintf "%S" description
  ; role = Test
  ; site = Source.site_at source ~offset
  ; comment = None
  ; lines = definition_lines_of source Test (Some definition)
  }
;;
