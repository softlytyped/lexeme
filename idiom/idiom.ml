open! Core

module Form = struct
  type t =
    | One_stage_pipeline
    | Boolean_branches
    | Eta_expansion
    | One_case_function
    | Discard_without_type
    | Length_comparison
    | Boolean_parameter
    | Nested_anonymous_function
  [@@deriving enumerate, equal, sexp_of]
end

type t =
  { site : Source.Site.t
  ; subject : string
  ; form : Form.t
  }

let create ~site ~subject form = { site; subject; form }
let site_of idiom = idiom.site
let subject_of idiom = idiom.subject
let form_of idiom = idiom.form
