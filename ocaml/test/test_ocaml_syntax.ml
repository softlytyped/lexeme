open! Core

let show text =
  Ocaml_syntax.read ~path:"input.ml" text
  |> Or_error.ok_exn
  |> Syntax.fold Display.algebra
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

let%expect_test "a chain of lets gives its links, and each bind or map its notation" =
  show
    "let run value =\n\
    \  let start = value and limit = 3 in\n\
    \  let%bind count = start >>= lookup in\n\
    \  let* total = count in\n\
    \  return (total + limit)";
  [%expect
    {|
    ((run Function
      (((value Variable)
        ((start Variable) (limit Variable) bind (count Variable) (bind) bind
         (total Variable) (chain (let and let%bind let*)))))))
    |}]
;;
