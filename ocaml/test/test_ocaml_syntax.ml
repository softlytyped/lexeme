open! Core

let%expect_test "functions, parameters and patterns all enter through bind" =
  let algebra : Sexp.t Binding_syntax.algebra =
    { bind =
        (fun binding ->
          [%sexp (binding.name : string), (binding.role : Binding_syntax.role)])
    ; enclose = (fun children -> Sexp.List children)
    }
  in
  let source =
    Binding_syntax.Source.source_of_string
      ~path:"input.ml"
      "let rec apply ~value = function Some (item as element) -> item | None -> value"
  in
  Ocaml_syntax.fold source algebra |> Or_error.ok_exn |> print_s;
  [%expect
    {| ((apply Function) ((value Variable) ((item Variable) (element Variable)))) |}]
;;
