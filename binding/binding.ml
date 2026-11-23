open! Core

module Codomain = struct
  type t =
    | Boolean
    | Optional_boolean
    | Fallible_boolean
    | Unit of string
    | Other of string
    | Unknown
end

module Role = struct
  type t =
    | Variable
    | Function of Codomain.t Lazy.t
end

type t =
  { name : string
  ; role : Role.t
  ; site : Source.Site.t
  ; comment : string option
  ; lines : int option
  }

let lines_of source role definition =
  match role with
  | Role.Variable -> None
  | Function _ -> Option.map definition ~f:(Source.code_lines_in source)
;;

let find source ~offset ?definition ~name role =
  let%map.Option () = Option.some_if (Source.does_spell source ~offset name) () in
  { name
  ; role
  ; site = Source.site_at source ~offset
  ; comment = Source.comment_after source ~offset:(offset + String.length name)
  ; lines = lines_of source role definition
  }
;;
