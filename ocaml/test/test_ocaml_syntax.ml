open! Core

let algebra : Sexp.t Syntax.algebra =
  { declare =
      (fun binding ->
        match binding.role with
        | Variable -> [%sexp (binding.name : string), "Variable"]
        | Function _ -> [%sexp (binding.name : string), "Function"])
  ; enclose = (fun children -> Sexp.List children)
  }
;;

let%expect_test "functions, parameters and patterns all enter through declare" =
  let text =
    "let rec apply ~value = function Some (item as element) -> item | None -> value"
  in
  Ocaml_syntax.read ~path:"input.ml" text
  |> Or_error.ok_exn
  |> Syntax.fold algebra
  |> print_s;
  [%expect
    {| ((apply Function) ((value Variable) ((item Variable) (element Variable)))) |}]
;;
