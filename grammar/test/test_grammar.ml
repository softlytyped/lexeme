open! Core

let lexicon = force Lexicon.english
let grammar = Grammar.of_lexicon lexicon

let print_readings ?(grammar = grammar) spellings =
  List.iter spellings ~f:(fun spelling ->
    let name = Grammar.Name.of_spelling spelling in
    let readings = Grammar.readings_of grammar name in
    let singulars = Grammar.singulars_of grammar name in
    print_s
      [%sexp
        (spelling : string)
        , (readings : Grammar.Reading.t list)
        , (singulars : string list)])
;;

let%expect_test "the grammar tells root verbs and predicates from noun heads" =
  print_readings
    [ "apply"
    ; "applies"
    ; "contract_exactly"
    ; "fully_contract"
    ; "test_and_set"
    ; "test_and"
    ; "test_or_exactly_contract"
    ; "does_test_and_set"
    ; "does_matrix"
    ; "is_sparse"
    ; "is_sparse_matrix"
    ; "has_entries"
    ; "is_ready"
    ; "exactly"
    ; "multiply"
    ; "glorp"
    ];
  [%expect
    {|
    (apply (Verb_phrase) ())
    (applies () ())
    (contract_exactly (Verb_phrase) ())
    (fully_contract (Verb_phrase) ())
    (test_and_set (Verb_phrase) ())
    (test_and () ())
    (test_or_exactly_contract (Verb_phrase) ())
    (does_test_and_set (Predicate) ())
    (does_matrix () ())
    (is_sparse (Predicate) ())
    (is_sparse_matrix (Predicate) ())
    (has_entries (Predicate) ())
    (is_ready (Predicate) ())
    (exactly () ())
    (multiply (Verb_phrase) ())
    (glorp () ())
    |}]
;;

let%expect_test "a preposition joins a subject to its object" =
  print_readings
    [ "matrix_of"
    ; "of_matrix"
    ; "entry_at"
    ; "entry_at_index"
    ; "of_entries"
    ; "sparse_matrices_of_indices"
    ; "matrix_of_indices"
    ; "matrix__of"
    ; "entries_of_at"
    ; "quickly_of"
    ; "matrix"
    ; "sparse_matrix"
    ; "register_bits"
    ; "strictly_positive_integer"
    ; "exclusive_or"
    ; "start_of_line"
    ];
  [%expect
    {|
    (matrix_of (Nominal) ())
    (of_matrix (Nominal) ())
    (entry_at (Nominal) ())
    (entry_at_index (Noun_phrase Nominal) ())
    (of_entries (Nominal) ())
    (sparse_matrices_of_indices (Noun_phrase Nominal) (sparse_matrix_of_indices))
    (matrix_of_indices (Noun_phrase Nominal) ())
    (matrix__of () ())
    (entries_of_at () (entry_of_at))
    (quickly_of () ())
    (matrix (Noun_phrase) ())
    (sparse_matrix (Noun_phrase) ())
    (register_bits (Noun_phrase) ())
    (strictly_positive_integer (Noun_phrase) ())
    (exclusive_or (Noun_phrase) ())
    (start_of_line (Noun_phrase Nominal) ())
    |}]
;;

module Frame = struct
  type t =
    { modifier : string
    ; plural : string
    ; singular : string
    ; complement : string
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Quickcheck.Generator.Let_syntax in
    let choose = Quickcheck.Generator.of_list in
    let%map modifier = choose [ ""; "sparse_"; "input_"; "strictly_positive_" ]
    and plural, singular =
      choose
        [ "entries", "entry"; "indices", "index"; "matrices", "matrix"; "bits", "bit" ]
    and complement = choose [ "_of"; "_at"; "_of_indices"; "_from_sparse_matrices" ] in
    { modifier; plural; singular; complement }
  ;;
end

let%test_unit "a singular reading keeps the whole frame around the head" =
  Quickcheck.test Frame.quickcheck_generator ~sexp_of:Frame.sexp_of_t ~f:(fun frame ->
    let spelling = frame.modifier ^ frame.plural ^ frame.complement in
    let singulars = Grammar.singulars_of grammar (Grammar.Name.of_spelling spelling) in
    [%test_result: string list]
      singulars
      ~expect:[ frame.modifier ^ frame.singular ^ frame.complement ])
;;

let%expect_test "a letter keeps its length under the spelling conventions of a binder" =
  List.iter [ "i"; "_i"; "x'"; "_λ'"; "io"; "βγ"; "_"; "0" ] ~f:(fun spelling ->
    let is_letter = Grammar.Name.is_letter (Grammar.Name.of_spelling spelling) in
    print_s [%sexp (spelling : string), (is_letter : bool)]);
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
  print_readings [ "_Sparse_MATRICES_at_Indices'"; "__Entries_OF'"; "Entries_of_entries" ];
  [%expect
    {|
    (_Sparse_MATRICES_at_Indices' (Noun_phrase Nominal)
     (_Sparse_MATRIX_at_Indices'))
    (__Entries_OF' (Nominal) (__Entry_OF'))
    (Entries_of_entries (Noun_phrase Nominal) (Entry_of_entries))
    |}]
;;

let%expect_test "a project can teach all four parts of speech and an irregular plural" =
  let vocabulary =
    Lexicon.Vocabulary.t_of_sexp
      (Sexp.of_string
         "((nouns (quux)) (verbs (frobnicate)) (adjectives (grue)) (adverbs (gruelike)) \
          (plurals ((quuxen quux) (sheep sheep))))")
  in
  let lexicon = Or_error.ok_exn (Lexicon.extend lexicon vocabulary) in
  print_readings
    ~grammar:(Grammar.of_lexicon lexicon)
    [ "quuxen_of"; "grue_quux_of"; "gruelike_frobnicate"; "sheep_of" ];
  [%expect
    {|
    (quuxen_of (Nominal) (quux_of))
    (grue_quux_of (Nominal) ())
    (gruelike_frobnicate (Verb_phrase) ())
    (sheep_of (Nominal) ())
    |}]
;;
