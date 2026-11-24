open! Core

(* The texts here never nest comments, so each opening pairs with the next closing. *)
let comments_in text =
  let offsets_of pattern = String.substr_index_all text ~may_overlap:false ~pattern in
  List.zip_exn (offsets_of "(*") (offsets_of "*)")
  |> List.map ~f:(fun (start, closing) ->
    let body = String.slice text (start + String.length "(*") closing in
    let extent = Source.Extent.create ~start ~stop:(closing + String.length "*)") in
    Source.Comment.create ~extent ~body)
;;

let source_of text = Source.create ~path:"input.ml" ~comments:(comments_in text) text
let extent_of text = Source.Extent.create ~start:0 ~stop:(String.length text)

let%expect_test "blank lines and lines of comment alone hold no code" =
  let text =
    "let apply value =\n\n\
    \  (* We explain why. *)\n\
    \  let doubled = value * 2 in (* a note *)\n\
    \  (* one\n\
    \     two *)\n\
    \  doubled\n"
  in
  print_s [%sexp (Source.code_lines_in (source_of text) (extent_of text) : int)];
  [%expect {| 3 |}]
;;

let%expect_test "a comment belongs to a name when it opens on the same line" =
  let text = "x (* lexeme: allow-letter *) y\n(* below *)\nz\t(* tab *) w" in
  let source = source_of text in
  List.iter [ "x"; "y"; "z"; "w" ] ~f:(fun name ->
    let offset = String.substr_index_exn text ~pattern:name + String.length name in
    print_s [%sexp (name : string), (Source.comment_after source ~offset : string option)]);
  [%expect
    {|
    (x (" lexeme: allow-letter "))
    (y ())
    (z (" tab "))
    (w ())
    |}]
;;

let%expect_test "positions count lines and columns from one" =
  let text = "first\nsecond line\n" in
  let source = source_of text in
  List.iter [ 0; 4; 6; 13 ] ~f:(fun offset ->
    let site = Source.site_at source ~offset in
    let line = Source.Site.line_of site
    and column = Source.Site.column_of site in
    print_s [%sexp (offset : int), (line : int), (column : int)]);
  [%expect {|
    (0 1 1)
    (4 1 5)
    (6 2 1)
    (13 2 8)
    |}]
;;

module Line = struct
  type t =
    | Code
    | Blank
    | Comment
  [@@deriving quickcheck, sexp_of]

  let spelling_of = function
    | Code -> "  value"
    | Blank -> "   "
    | Comment -> "  (* a comment *)"
  ;;
end

let%test_unit "only lines of code count, whatever surrounds them" =
  Quickcheck.test
    (List.quickcheck_generator Line.quickcheck_generator)
    ~sexp_of:[%sexp_of: Line.t list]
    ~f:(fun lines ->
      let text = String.concat ~sep:"\n" (List.map lines ~f:Line.spelling_of) in
      [%test_result: int]
        (Source.code_lines_in (source_of text) (extent_of text))
        ~expect:
          (List.count lines ~f:(function
            | Code -> true
            | Blank | Comment -> false)))
;;
