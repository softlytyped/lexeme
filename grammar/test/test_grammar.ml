open! Core

let lexicon = force Lexicon.english

let%expect_test "the grammar distinguishes root verbs, noun heads and their modifiers" =
  let read role names =
    List.iter names ~f:(fun name ->
      let reading =
        Grammar.read lexicon role name |> Option.map ~f:Grammar.singulars_of
      in
      print_s [%sexp (name : string), (reading : string list option)])
  in
  read
    Function
    [ "apply"
    ; "applies"
    ; "contract_exactly"
    ; "fully_contract"
    ; "test_and_set"
    ; "test_and"
    ; "test_or_exactly_contract"
    ; "does_test_and_set"
    ; "does_matrix"
    ; "matrix_of"
    ; "of_matrix"
    ; "entry_at"
    ; "entry_at_index"
    ; "is_sparse"
    ; "is_sparse_matrix"
    ; "has_entries"
    ; "sparse_matrices_of_indices"
    ; "of_entries"
    ; "matrix_of_indices"
    ; "matrix__of"
    ; "entries_of_at"
    ; "quickly_of"
    ];
  read
    Variable
    [ "matrix"
    ; "sparse_matrix"
    ; "register_bits"
    ; "strictly_positive_integer"
    ; "start_of_line"
    ; "exactly"
    ; "multiply"
    ; "is_ready"
    ; "glorp"
    ];
  [%expect
    {|
    (apply (()))
    (applies ())
    (contract_exactly (()))
    (fully_contract (()))
    (test_and_set (()))
    (test_and ())
    (test_or_exactly_contract (()))
    (does_test_and_set (()))
    (does_matrix ())
    (matrix_of (()))
    (of_matrix (()))
    (entry_at (()))
    (entry_at_index (()))
    (is_sparse (()))
    (is_sparse_matrix (()))
    (has_entries (()))
    (sparse_matrices_of_indices ((sparse_matrix_of_indices)))
    (of_entries (()))
    (matrix_of_indices (()))
    (matrix__of ())
    (entries_of_at ())
    (quickly_of ())
    (matrix (()))
    (sparse_matrix (()))
    (register_bits (()))
    (strictly_positive_integer (()))
    (start_of_line (()))
    (exactly ())
    (multiply ())
    (is_ready ())
    (glorp ())
    |}]
;;

let%expect_test "a singular reading preserves the whole frame around the head" =
  let choose choices = Quickcheck.Generator.of_list choices in
  let generator =
    let open Quickcheck.Generator.Let_syntax in
    let%bind modifier = choose [ ""; "sparse_"; "input_"; "strictly_positive_" ] in
    let%bind plural, singular =
      choose
        [ "entries", "entry"; "indices", "index"; "matrices", "matrix"; "bits", "bit" ]
    in
    let%map complement =
      choose [ "_of"; "_at"; "_of_indices"; "_from_sparse_matrices" ]
    in
    modifier ^ plural ^ complement, modifier ^ singular ^ complement
  in
  Quickcheck.test
    generator
    ~sexp_of:[%sexp_of: string * string]
    ~f:(fun (plural, singular) ->
      [%test_result: bool]
        (Grammar.read lexicon Function plural
         |> Option.exists ~f:(fun reading ->
           List.mem (Grammar.singulars_of reading) singular ~equal:String.equal))
        ~expect:true);
  [%expect {| |}]
;;

let%expect_test "a letter keeps its length under the spelling conventions of a binder" =
  List.iter [ "i"; "_i"; "x'"; "_λ'"; "io"; "βγ"; "_"; "0" ] ~f:(fun name ->
    print_s [%sexp (name : string), (Grammar.is_letter name : bool)]);
  [%expect
    {|
    (i true)
    (_i true)
    (x' true)
    ("_\206\187'" true)
    (io false)
    ("\206\178\206\179" false)
    (_ false)
    (0 false)
    |}]
;;

let%expect_test "a project can teach all four parts of speech and an irregular plural" =
  let vocabulary =
    Lexicon.vocabulary_of_sexp
      (Sexp.of_string
         "((nouns (quux)) (verbs (frobnicate)) (adjectives (grue)) (adverbs (gruelike)) \
          (plurals ((quuxen quux))))")
  in
  let vocabulary = Or_error.ok_exn (Lexicon.extend lexicon vocabulary) in
  List.iter [ "quuxen_of"; "grue_quux_of"; "gruelike_frobnicate" ] ~f:(fun name ->
    print_s
      [%sexp
        (Grammar.read vocabulary Function name |> Option.map ~f:Grammar.singulars_of
         : string list option)]);
  [%expect {|
    ((quux_of))
    (())
    (())
    |}]
;;
