open! Core

module Link = struct
  module Form = struct
    type t =
      | Plain_binding
      | Monad_binding
    [@@deriving equal]
  end

  module Definition = struct
    type t =
      | Pipeline
      | Other_expression
  end

  type t =
    { site : Source.Site.t
    ; keyword : string
    ; form : Form.t
    ; definition : Definition.t
    ; names : string list
    ; reads : string list
    }

  let create ~site ~keyword ~form ~definition ~names ~reads =
    { site; keyword; form; definition; names; reads }
  ;;

  let site_of link = link.site
  let keyword_of link = link.keyword
  let form_of link = link.form
  let definition_of link = link.definition
  let names_of link = link.names
  let reads_of link = link.reads
end

type t = Link.t list

let create links = links
let links_of chain = chain
