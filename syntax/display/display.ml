open! Core

let role_of (binding : Binding.t) =
  match binding.role with
  | Variable -> "Variable"
  | Function _ -> "Function"
  | Field _ -> "Field"
  | Type -> "Type"
  | Constructor -> "Constructor"
  | Test -> "Test"
;;

let declaration_of (binding : Binding.t) =
  [%sexp (binding.name : string), (role_of binding : string)]
;;

let definition_of (binding : Binding.t) children =
  [%sexp (binding.name : string), (role_of binding : string), (children : Sexp.t list)]
;;

let algebra : Sexp.t Syntax.algebra =
  { declare = declaration_of
  ; decide = (fun decision -> [%sexp (decision.keyword : string)])
  ; enclose = (fun children -> Sexp.List children)
  ; define = definition_of
  }
;;
