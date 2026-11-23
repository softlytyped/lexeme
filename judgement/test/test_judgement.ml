open! Core

let algebra =
  Judgement.algebra_of
    (force Lexicon.english)
    ~limits:Judgement.Limits.default
    ~exemptions:(String.Set.singleton "main")
;;

let diagnostics_of ?(path = "input.ml") text =
  Ocaml_syntax.read ~path text
  |> Or_error.ok_exn
  |> Syntax.fold algebra
  |> Judgement.diagnostics_of
;;

let check ?path text =
  List.iter (diagnostics_of ?path text) ~f:(fun diagnostic ->
    print_endline (Diagnostic.to_string diagnostic))
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
    let subjects =
      diagnostics_of (String.concat ~sep:"\n" permutation)
      |> List.map ~f:(fun diagnostic -> diagnostic.subject)
    in
    [%test_result: string list] subjects ~expect:[ "entries_at" ]);
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

let%expect_test "local declarations share a continuation and stop at sibling operands" =
  check
    {|let apply value =
  ( (let entries_of items = items in entries_of value)
  , (let entry_of item = item in entry_of value) )
let run value =
  let entry_at item = item in
  let%bind count = value in
  let entries_at items = items in
  entries_at count|};
  [%expect
    {| input.ml:7:7: note[plural-lift] entries_at: check whether this is a map-like lift of entry_at (line 5) |}]
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

let%expect_test "a predicate is judged by its type through aliases and shadowing" =
  check
    ~path:"decisions.mli"
    {|type decision = bool option
type operation = int -> decision
val is_ready : operation
type bool = int
val has_rows : unit -> bool
module Bool : sig type t = string end
val is_empty : unit -> Bool.t|};
  check {|let apply value =
  let does_reduce = String.length in
  does_reduce value|};
  [%expect
    {|
    decisions.mli:3:5: note[predicate-state] is_ready: Are you sure Some false is a meaningfully different state from None? If so, you can ignore this note.
    decisions.mli:4:6: error[type-name] bool: expected a noun phrase
    decisions.mli:5:5: error[predicate-result] has_rows: expected a Boolean result, found int
    decisions.mli:7:5: error[predicate-result] is_empty: expected a Boolean result, found string
    input.ml:2:7: error[predicate-result] does_reduce: expected a Boolean result, found int
    |}]
;;

let%expect_test "a function's name agrees with what it gives" =
  check
    ~path:"agreement.mli"
    {|val contains : int list -> int -> bool
val is_empty : int list -> bool
val entries_of : int -> unit
val of_int : int -> unit
val print : int -> unit
val size_of : int list -> int|};
  check {|let matches left right = left = right
let report count = ignore count|};
  [%expect
    {|
    agreement.mli:1:5: error[predicate-name] contains: expected a predicate such as is_empty or has_entries for a Boolean
    agreement.mli:3:5: error[command-name] entries_of: expected a root verb phrase for a function that gives nothing
    agreement.mli:4:5: error[command-name] of_int: expected a root verb phrase for a function that gives nothing
    input.ml:1:5: error[predicate-name] matches: expected a predicate such as is_empty or has_entries for a Boolean
    |}]
;;

(* A function of [lines] lines of code, with a blank line and a comment between each,
   which do not count. *)
let function_of_length lines =
  let steps = List.init (lines - 2) ~f:(fun _ -> "  ignore 0;\n\n  (* a step *)\n") in
  "let apply () =\n" ^ String.concat steps ^ "  ()\n"
;;

let%expect_test "a function past each limit earns a note, a warning and then an error" =
  List.iter [ 10; 11; 20; 21; 30; 31 ] ~f:(fun lines -> check (function_of_length lines));
  [%expect
    {|
    input.ml:1:5: note[function-length] apply: 11 lines of code, more than 10
    input.ml:1:5: note[function-length] apply: 20 lines of code, more than 10
    input.ml:1:5: warning[function-length] apply: 21 lines of code, more than 20
    input.ml:1:5: warning[function-length] apply: 30 lines of code, more than 20
    input.ml:1:5: error[function-length] apply: 31 lines of code, more than 30
    |}]
;;

let%expect_test "an exemption spares a name but not its length" =
  check {|let main () = ()|};
  check
    (String.substr_replace_first (function_of_length 11) ~pattern:"apply" ~with_:"main");
  [%expect {| input.ml:1:5: note[function-length] main: 11 lines of code, more than 10 |}]
;;

let%expect_test "limits must rise from note to warning to error" =
  List.iter
    [ "((note 10) (warning 20) (error 30))"
    ; "((note 20) (warning 10) (error 30))"
    ; "((note 0) (warning 0) (error 0))"
    ]
    ~f:(fun text ->
      let limits =
        Or_error.try_with (fun () -> Judgement.Limits.t_of_sexp (Sexp.of_string text))
      in
      print_s [%sexp (Result.is_ok limits : bool)]);
  [%expect {|
    true
    false
    false
    |}]
;;

let%expect_test "types and fields are nouns, and a field that holds a Boolean is a \
                 predicate"
  =
  check
    ~path:"shapes.mli"
    {|type t
type matrix = { rows : int; is_square : bool; ready : bool; transpose : int -> int }
type quickly
type x
type verdict = Accepted | Rejected | Strictly_positive | Quickly | Late of int|};
  [%expect
    {|
    shapes.mli:2:47: error[predicate-name] ready: expected a predicate such as is_empty or has_entries for a Boolean
    shapes.mli:3:6: error[type-name] quickly: expected a noun phrase
    shapes.mli:4:6: error[single-letter] x: use a longer name or add a lexeme: allow-letter comment with a reason
    shapes.mli:5:58: error[constructor-name] Quickly: expected a noun phrase or an adjective phrase
    |}]
;;

let%expect_test "a function body branches on Booleans with at most one if-else" =
  check
    {|let decide flag = match flag with true -> 1 | false -> 0
let choose flag = if flag then 1 else 0
let compare first second = if first then 1 else if second then 2 else 3
let tabulate first second = match first, second with true, _ -> 1 | _, true -> 2 | _ -> 3
let pick flag value = match flag, value with true, Some _ -> 1 | _ -> 0
let count items = match items with [] -> 0 | _ :: _ -> 1|};
  [%expect
    {|
    input.ml:1:19: warning[lone-boolean-match] match: the one Boolean decision of its function reads as an if-else
    input.ml:1:19: note[boolean-match] match: could a variant name the cases of this Boolean?
    input.ml:3:5: warning[if-else] compare: has if-else on lines 3, 3: keep at most one, and match the rest
    input.ml:4:29: error[boolean-tuple] match: matches 2 Booleans at once: parse, don't validate, with a variant
    input.ml:5:23: note[boolean-match] match: could a variant name the cases of this Boolean?
    |}]
;;

let%expect_test "a guard after work that it does not use asks to come first" =
  check
    {|let total items limit =
  let sum = List.fold items ~init:0 ~f:( + ) in
  match limit with
  | None -> Error "no limit"
  | Some bound -> Ok (min sum bound)
let scale items factor =
  let sum = List.fold items ~init:0 ~f:( + ) in
  if sum > factor then sum else factor
let check items =
  let increment item = item + 1 in
  if List.is_empty items then Error "empty" else Ok (List.map items ~f:increment)|};
  [%expect
    {|
    input.ml:3:3: note[late-guard] match: this precondition uses none of the work before it: could it come first?
    input.ml:10:7: error[function-name] increment: expected a root verb phrase, a prepositional name or a predicate
    |}]
;;

let%expect_test "an inline test has more room than a function" =
  let test_of_length lines =
    let steps = List.init (lines - 2) ~f:(fun _ -> "  ignore 0;\n") in
    "let%expect_test \"example\" =\n" ^ String.concat steps ^ "  ()\n"
  in
  List.iter [ 15; 16 ] ~f:(fun lines -> check (test_of_length lines));
  [%expect
    {| input.ml:1:1: note[function-length] "example": 16 lines of code, more than 15 |}]
;;
