open! Core

type t =
  { site : Source.Site.t
  ; field : string
  ; owner : string
  }

let create ~site ~field ~owner = { site; field; owner }
let site_of access = access.site
let field_of access = access.field
let owner_of access = access.owner
