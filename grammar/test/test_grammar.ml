open! Core

let lexicon = force Lexicon.english
let grammar = Grammar.of_lexicon lexicon

(* Prints each spelling with its readings and the singular spellings of its subject. *)
let show ?(grammar = grammar) spellings =
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

let%expect_test "a root verb takes adverbs" =
  show [ "apply"; "contract_exactly"; "fully_contract"; "multiply" ];
  [%expect
    {|
    (apply (Verb_phrase) ())
    (contract_exactly (Verb_phrase) ())
    (fully_contract (Verb_phrase) ())
    (multiply (Verb_phrase) ())
    |}]
;;

let%expect_test "conjunctions join root verbs, and an inflected verb is no root" =
  show [ "test_and_set"; "test_or_exactly_contract"; "test_and"; "applies" ];
  [%expect
    {|
    (test_and_set (Verb_phrase) ())
    (test_or_exactly_contract (Verb_phrase) ())
    (test_and () ())
    (applies () ())
    |}]
;;

let%expect_test "an auxiliary verb opens a predicate" =
  show [ "is_sparse"; "is_sparse_matrix"; "has_entries"; "is_ready" ];
  [%expect
    {|
    (is_sparse (Predicate) ())
    (is_sparse_matrix (Predicate) ())
    (has_entries (Predicate) ())
    (is_ready (Predicate) ())
    |}]
;;

let%expect_test "a predicate's complement must fit its auxiliary" =
  show [ "does_test_and_set"; "does_drain_into"; "does_matrix" ];
  [%expect
    {|
    (does_test_and_set (Predicate) ())
    (does_drain_into (Predicate) ())
    (does_matrix () ())
    |}]
;;

let%expect_test "a word outside the lexicon makes no phrase" =
  show [ "exactly"; "glorp"; "matrix__of"; "entries_of_at"; "quickly_of" ];
  [%expect
    {|
    (exactly () ())
    (glorp () ())
    (matrix__of () ())
    (entries_of_at () (entry_of_at))
    (quickly_of () ())
    |}]
;;

let%expect_test "a preposition joins a subject to its object" =
  show [ "matrix_of"; "of_matrix"; "entry_at"; "entry_at_index" ];
  [%expect
    {|
    (matrix_of (Nominal) ())
    (of_matrix (Nominal) ())
    (entry_at (Nominal) ())
    (entry_at_index (Noun_phrase Nominal) ())
    |}]
;;

let%expect_test "a plural subject gives its singular spellings" =
  show [ "of_entries"; "sparse_matrices_of_indices"; "matrix_of_indices" ];
  [%expect
    {|
    (of_entries (Nominal) ())
    (sparse_matrices_of_indices (Noun_phrase Nominal) (sparse_matrix_of_indices))
    (matrix_of_indices (Noun_phrase Nominal) ())
    |}]
;;

let%expect_test "a noun phrase is nouns and adjectives, ending in a noun" =
  show [ "matrix"; "sparse_matrix"; "register_bits" ];
  [%expect
    {|
    (matrix (Noun_phrase) ())
    (sparse_matrix (Noun_phrase) ())
    (register_bits (Noun_phrase) ())
    |}]
;;

let%expect_test "adverbs modify an adjective, and prepositions join noun phrases" =
  show [ "strictly_positive_integer"; "exclusive_or"; "start_of_line" ];
  [%expect
    {|
    (strictly_positive_integer (Noun_phrase) ())
    (exclusive_or (Noun_phrase) ())
    (start_of_line (Noun_phrase Verb_phrase Nominal) ())
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

  let generator =
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
  Quickcheck.test Frame.generator ~sexp_of:Frame.sexp_of_t ~f:(fun frame ->
    let spelling = frame.modifier ^ frame.plural ^ frame.complement in
    let singulars = Grammar.singulars_of grammar (Grammar.Name.of_spelling spelling) in
    [%test_result: string list]
      singulars
      ~expect:[ frame.modifier ^ frame.singular ^ frame.complement ])
;;

let inspect spellings =
  List.iter spellings ~f:(fun spelling ->
    let name = Grammar.Name.of_spelling spelling in
    print_s [%sexp (spelling : string), (Grammar.Name.is_letter name : bool)])
;;

let%expect_test "a letter keeps its length under underscores and primes" =
  inspect [ "i"; "_i"; "x'"; "_λ'" ];
  [%expect {|
    (i true)
    (_i true)
    (x' true)
    ("_\206\187'" true)
    |}]
;;

let%expect_test "a longer name, an underscore alone and a digit are no letters" =
  inspect [ "io"; "βγ"; "_"; "0" ];
  [%expect
    {|
    (io false)
    ("\206\178\206\179" false)
    (_ false)
    (0 false)
    |}]
;;

let%expect_test "a singular keeps the spelling outside its head" =
  show [ "_Sparse_MATRICES_at_Indices'"; "__Entries_OF'"; "Entries_of_entries" ];
  [%expect
    {|
    (_Sparse_MATRICES_at_Indices' (Noun_phrase Nominal)
     (_Sparse_MATRIX_at_Indices'))
    (__Entries_OF' (Nominal) (__Entry_OF'))
    (Entries_of_entries (Noun_phrase Nominal) (Entry_of_entries))
    |}]
;;

let grammar_with vocabulary =
  let vocabulary = Lexicon.Vocabulary.t_of_sexp (Sexp.of_string vocabulary) in
  Grammar.of_lexicon (Or_error.ok_exn (Lexicon.extend lexicon vocabulary))
;;

let%expect_test "a project can teach nouns and irregular plurals" =
  let grammar = grammar_with "((nouns (quux)) (plurals ((quuxen quux) (sheep sheep))))" in
  show ~grammar [ "quuxen_of"; "sheep_of" ];
  [%expect {|
    (quuxen_of (Nominal) (quux_of))
    (sheep_of (Nominal) ())
    |}]
;;

let%expect_test "a project can teach verbs, adjectives and adverbs" =
  let grammar =
    grammar_with "((verbs (frobnicate)) (adjectives (grue)) (adverbs (gruelike)))"
  in
  show ~grammar [ "grue_matrix_of"; "gruelike_frobnicate" ];
  [%expect
    {|
    (grue_matrix_of (Nominal) ())
    (gruelike_frobnicate (Verb_phrase) ())
    |}]
;;

let%expect_test "an adjective's comparative and superlative are adjectives too" =
  show [ "nearest_file"; "smaller_root"; "happiest_path"; "biggest_entry" ];
  [%expect
    {|
    (nearest_file (Noun_phrase Verb_phrase) ())
    (smaller_root (Noun_phrase) ())
    (happiest_path (Noun_phrase) ())
    (biggest_entry (Noun_phrase) ())
    |}]
;;
