open! Core

let role_of binding =
  match Binding.role_of binding with
  | Variable -> "Variable"
  | Function _ -> "Function"
  | Field _ -> "Field"
  | Type -> "Type"
  | Constructor -> "Constructor"
  | Test -> "Test"
;;

let declaration_of binding =
  [%sexp (Binding.name_of binding : string), (role_of binding : string)]
;;

let definition_of binding children =
  [%sexp
    (Binding.name_of binding : string)
    , (role_of binding : string)
    , (children : Sexp.t list)]
;;

let algebra : Sexp.t Syntax.algebra =
  { declare = declaration_of
  ; decide = (fun decision -> [%sexp (Decision.keyword_of decision : string)])
  ; access = (fun access -> [%sexp "access", (Access.field_of access : string)])
  ; enclose = (fun children -> Sexp.List children)
  ; define = definition_of
  }
;;
