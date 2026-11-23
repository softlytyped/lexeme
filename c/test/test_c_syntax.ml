open! Core

let show text =
  C_syntax.read ~path:"input.c" ~directories:[] ~definitions:[] text
  |> Or_error.ok_exn
  |> Syntax.fold Display.algebra
  |> print_s
;;

let%expect_test "a structure's members form a scope, and a function's body follows it" =
  show
    "struct pair { int first; int second; };\n\
     int sum(struct pair given) { int total = given.first; return total; }\n\
     void report(int count);";
  [%expect
    {|
    ((pair Type) ((first Field) (second Field))
     (sum Function ((given Variable) ((total Variable)))) (report Function)
     ((count Variable)))
    |}]
;;

let%expect_test "enumeration constants and type names share the enclosing scope" =
  show "enum colour { red, green };\ntypedef enum colour hue;";
  [%expect {| ((colour Type) (red Constructor) (green Constructor) (hue Type)) |}]
;;
