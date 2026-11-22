open! Core

let interpret ?(path = "input.ml") text algebra =
  let source = Binding_syntax.Source.source_of_string ~path text in
  Ocaml_syntax.fold source algebra
;;

let check ?path text =
  let file =
    interpret ?path text (Naming.algebra_of (force Lexicon.english)) |> Or_error.ok_exn
  in
  List.iter (Naming.diagnostics_of file) ~f:(fun diagnostic ->
    print_endline (Naming.format diagnostic))
;;

let%expect_test "only a plural subject and a singular function in its scope make a note" =
  check
    {|let sparse_matrices_at index = index
let sparse_matrix_at index = index
let matrix_of_indices index = index
let matrix_of_index index = index
module Other = struct let entries_of items = items end
let entry_of item = item
let indices_of items = items
let index_of = 0
let apply value =
  let bits_of items = items in
  let bit_of item = item in
  match value with
  | Some number -> let words_at items = items in words_at number
  | None -> let word_at item = item in word_at value
|};
  [%expect
    {|
    input.ml:1:5: note[plural-lift] sparse_matrices_at: check whether this is a map-like lift of sparse_matrix_at (line 2)
    input.ml:8:5: error[variable-name] index_of: expected a noun phrase
    input.ml:10:7: note[plural-lift] bits_of: check whether this is a map-like lift of bit_of (line 11)
    |}]
;;

let%expect_test "a letter exception belongs to the marked binder alone" =
  check
    {|let apply x (* lexeme: allow-letter published coordinate *) y = x
let bind (z (* lexeme: allow-letter *) , value) = value
let explain _i' = "(* lexeme: allow-letter in a string *)"
let run ~f = fun v -> let%bind (j, k) = f v in j
let store = let (x, _ as y) = (0, 0) in x + y
let fold items = for i = 0 to 1 do () done
|};
  [%expect
    {|
    input.ml:1:61: error[single-letter] y: use a longer name or add a lexeme: allow-letter comment with a reason
    input.ml:2:11: error[single-letter] z: use a longer name or add a lexeme: allow-letter comment with a reason
    input.ml:3:13: error[single-letter] _i': use a longer name or add a lexeme: allow-letter comment with a reason
    input.ml:4:10: error[single-letter] f: use a longer name or add a lexeme: allow-letter comment with a reason
    input.ml:4:18: error[single-letter] v: use a longer name or add a lexeme: allow-letter comment with a reason
    input.ml:4:33: error[single-letter] j: use a longer name or add a lexeme: allow-letter comment with a reason
    input.ml:4:36: error[single-letter] k: use a longer name or add a lexeme: allow-letter comment with a reason
    input.ml:5:18: error[single-letter] x: use a longer name or add a lexeme: allow-letter comment with a reason
    input.ml:5:26: error[single-letter] y: use a longer name or add a lexeme: allow-letter comment with a reason
    input.ml:6:22: error[single-letter] i: use a longer name or add a lexeme: allow-letter comment with a reason
    |}]
;;

let%expect_test "signatures, typed aliases, noun variables and root verbs" =
  check
    ~path:"input.mli"
    {|val matrices_of : int -> int list
val matrix_of : int -> int
val does_matrix : unit -> bool
val multiply : int
val x (* lexeme: allow-letter published coefficient *) : int
module type Separate = sig val indices_at : int -> int list end
module type Another = sig val index_at : int -> int end|};
  check
    {|let does_matrix : int -> int = Library.reduce
let applies value = value
let apply (multiply, exactly) = multiply
let ( @@ ) left right = left right
|};
  [%expect
    {|
    input.mli:1:5: note[plural-lift] matrices_of: check whether this is a map-like lift of matrix_of (line 2)
    input.mli:3:5: error[function-name] does_matrix: expected a root verb phrase, a prepositional name or a predicate
    input.mli:4:5: error[variable-name] multiply: expected a noun phrase
    input.ml:1:5: error[function-name] does_matrix: expected a root verb phrase, a prepositional name or a predicate
    input.ml:1:5: error[predicate-result] does_matrix: expected a Boolean result, found int
    input.ml:2:5: error[function-name] applies: expected a root verb phrase, a prepositional name or a predicate
    input.ml:3:12: error[variable-name] multiply: expected a noun phrase
    input.ml:3:22: error[variable-name] exactly: expected a noun phrase
    |}]
;;

let%expect_test "declaration order preserves the set of plural notices" =
  let declarations =
    [ "let entries_at items = items"
    ; "let entry_at item = item"
    ; "let rows_at items = items"
    ]
  in
  Quickcheck.test (List.gen_permutations declarations) ~f:(fun permutation ->
    let file =
      interpret
        (String.concat ~sep:"\n" permutation)
        (Naming.algebra_of (force Lexicon.english))
      |> Or_error.ok_exn
    in
    let notes =
      Naming.diagnostics_of file
      |> List.map ~f:(fun diagnostic -> diagnostic.binding.name)
    in
    [%test_result: string list] notes ~expect:[ "entries_at" ]);
  [%expect {| |}]
;;

let%expect_test "the predicate result is the final codomain, including any extra state" =
  check
    ~path:"decisions.mli"
    {|val is_ready : (int -> string) -> count:int -> bool
val has_entry : int -> bool option
val does_accept : int -> bool Or_error.t
val is_enabled : int -> (bool, Error.t) result
val is_zero : int -> int
val has_rows : int -> int option
val has_width : int -> bool list
val does_apply : (int -> bool) -> string
val apply : int -> bool option|};
  check
    {|let is_ready value : bool option = Some value
let does_accept value : bool Or_error.t = Ok value
let has_value value : int = value|};
  [%expect
    {|
    decisions.mli:2:5: note[predicate-state] has_entry: Are you sure Some false is a meaningfully different state from None? If so, you can ignore this note.
    decisions.mli:3:5: note[predicate-state] does_accept: Are you sure Ok false is a meaningfully different state from Error? If so, you can ignore this note.
    decisions.mli:4:5: note[predicate-state] is_enabled: Are you sure Ok false is a meaningfully different state from Error? If so, you can ignore this note.
    decisions.mli:5:5: error[predicate-result] is_zero: expected a Boolean result, found int
    decisions.mli:6:5: error[predicate-result] has_rows: expected a Boolean result, found int option
    decisions.mli:7:5: error[predicate-result] has_width: expected a Boolean result, found bool list
    decisions.mli:8:5: error[predicate-result] does_apply: expected a Boolean result, found string
    input.ml:1:5: note[predicate-state] is_ready: Are you sure Some false is a meaningfully different state from None? If so, you can ignore this note.
    input.ml:2:5: note[predicate-state] does_accept: Are you sure Ok false is a meaningfully different state from Error? If so, you can ignore this note.
    input.ml:3:5: error[predicate-result] has_value: expected a Boolean result, found int
    |}]
;;

let%expect_test "a value shadows a singular function in the same lexical scope" =
  check {|let entry_of item = item
let entry_of = 0
let entries_of items = items|};
  [%expect {| input.ml:2:5: error[variable-name] entry_of: expected a noun phrase |}]
;;

let%expect_test "an object field and a typed callback parameter bind variables" =
  check
    {|let apply (multiply : int -> int) number = multiply number
let table = object val mutable x = 0 method is_empty = true end|};
  [%expect
    {|
    input.ml:1:12: error[variable-name] multiply: expected a noun phrase
    input.ml:2:32: error[single-letter] x: use a longer name or add a lexeme: allow-letter comment with a reason
    |}]
;;
