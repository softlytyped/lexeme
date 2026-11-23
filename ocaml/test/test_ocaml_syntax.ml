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

let algebra : Sexp.t Syntax.algebra =
  { declare = (fun binding -> [%sexp (binding.name : string), (role_of binding : string)])
  ; decide = (fun decision -> [%sexp (decision.keyword : string)])
  ; enclose = (fun children -> Sexp.List children)
  ; define =
      (fun binding children ->
        [%sexp
          (binding.name : string), (role_of binding : string), (children : Sexp.t list)])
  }
;;

let show text =
  Ocaml_syntax.read ~path:"input.ml" text
  |> Or_error.ok_exn
  |> Syntax.fold algebra
  |> print_s
;;

let%expect_test "a function defines its body, where its parameters, patterns and \
                 decisions are"
  =
  show "let rec apply ~value = function Some (item as element) -> item | None -> value";
  [%expect
    {|
    ((apply Function
      ((function (value Variable) ((item Variable) (element Variable))))))
    |}]
;;

let%expect_test "types declare their constructors and fields in a scope of their own" =
  show "type shape = Circle of { radius : float } | Square\nlet%test_unit \"check\" = ()";
  [%expect
    {|
    ((shape Type) ((Circle Constructor) (radius Field) (Square Constructor))
     ("\"check\"" Test ()))
    |}]
;;
