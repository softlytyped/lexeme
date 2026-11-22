open! Core

let lexicon = force Lexicon.english
let grammar = Grammar.grammar_of lexicon

let%expect_test "the grammar distinguishes root verbs, noun heads and their modifiers" =
  let read interpretation names =
    List.iter names ~f:(fun name ->
      let reading = interpretation (Grammar.read name) in
      print_s [%sexp (name : string), (reading : string list option)])
  in
  read
    (Grammar.function_of grammar)
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
    (fun phrase -> Option.some_if (Grammar.is_noun grammar phrase) [])
    [ "matrix"
    ; "sparse_matrix"
    ; "register_bits"
    ; "strictly_positive_integer"
    ; "exclusive_or"
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
    (exclusive_or (()))
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
        (Grammar.function_of grammar (Grammar.read plural)
         |> Option.exists ~f:(List.exists ~f:(String.equal singular)))
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

let%expect_test "a singular keeps the spelling outside its head" =
  List.iter
    [ "_Sparse_MATRICES_at_Indices'"; "__Entries_OF'"; "Entries_of_entries" ]
    ~f:(fun name ->
      print_s
        [%sexp (Grammar.function_of grammar (Grammar.read name) : string list option)]);
  [%expect
    {|
    ((_Sparse_MATRIX_at_Indices'))
    ((__Entry_OF'))
    ((Entry_of_entries))
    |}]
;;

let%expect_test "a project can teach all four parts of speech and an irregular plural" =
  let vocabulary =
    Lexicon.vocabulary_of_sexp
      (Sexp.of_string
         "((nouns (quux)) (verbs (frobnicate)) (adjectives (grue)) (adverbs (gruelike)) \
          (plurals ((quuxen quux) (sheep sheep))))")
  in
  let vocabulary = Or_error.ok_exn (Lexicon.extend lexicon vocabulary) in
  List.iter
    [ "quuxen_of"; "grue_quux_of"; "gruelike_frobnicate"; "sheep_of" ]
    ~f:(fun name ->
      print_s
        [%sexp
          (Grammar.function_of (Grammar.grammar_of vocabulary) (Grammar.read name)
           : string list option)]);
  [%expect {|
    ((quux_of))
    (())
    (())
    (())
    |}]
;;
