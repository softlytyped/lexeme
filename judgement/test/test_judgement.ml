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

(* Prints each finding as its position, its rule and its subject. *)
let show ?path text =
  List.iter (diagnostics_of ?path text) ~f:(fun diagnostic ->
    let line = Source.Site.line_of (Diagnostic.site_of diagnostic)
    and column = Source.Site.column_of (Diagnostic.site_of diagnostic) in
    printf
      "%d:%d %s %s\n"
      line
      column
      (Diagnostic.code_of diagnostic)
      (Diagnostic.subject_of diagnostic))
;;

(* Prints each finding in full. *)
let print ?path text =
  List.iter (diagnostics_of ?path text) ~f:(fun diagnostic ->
    print_endline (Diagnostic.to_string diagnostic))
;;

let%expect_test "a plural meets a singular function in its own scope" =
  show
    {|let sparse_matrices_at index = index
let sparse_matrix_at index = index
let matrix_of_indices index = index
let matrix_of_index index = index
module Other = struct let entries_of items = items end
let entry_of item = item
let indices_of items = items
let index_of = 0|};
  [%expect
    {|
    1:5 plural-lift sparse_matrices_at
    8:5 variable-name index_of
    |}]
;;

let%expect_test "a local plural meets a singular in its own branch" =
  show
    {|let apply value =
  let bits_of items = items in
  let bit_of item = item in
  match value with
  | Some number -> let words_at items = items in words_at number
  | None -> let word_at item = item in word_at value|};
  [%expect {| 2:7 plural-lift bits_of |}]
;;

let%expect_test "declaration order preserves the set of plural notes" =
  let declarations =
    [ "let entries_at items = items"
    ; "let entry_at item = item"
    ; "let rows_at items = items"
    ]
  in
  Quickcheck.test (List.gen_permutations declarations) ~f:(fun permutation ->
    let diagnostics = diagnostics_of (String.concat ~sep:"\n" permutation) in
    let subjects = List.map diagnostics ~f:Diagnostic.subject_of in
    [%test_result: string list] subjects ~expect:[ "entries_at" ]);
  [%expect {| |}]
;;

let%expect_test "a value shadows a singular function in the same lexical scope" =
  show {|let entry_of item = item
let entry_of = 0
let entries_of items = items|};
  [%expect {| 2:5 variable-name entry_of |}]
;;

let%expect_test "local declarations share a continuation and stop at sibling operands" =
  show
    {|let apply value =
  ( (let entries_of items = items in entries_of value)
  , (let entry_of item = item in entry_of value) )
let run value =
  let entry_at item = item in
  let%bind count = value in
  let entries_at items = count :: items in
  entries_at [ count ]|};
  [%expect {|
    6:12 lone-bind let%bind
    7:7 plural-lift entries_at
    |}]
;;

let%expect_test "a letter exception belongs to the marked binder alone" =
  show
    {|let apply x (* lexeme: allow-letter published coordinate *) y = x
let bind (z (* lexeme: allow-letter *) , value) = value
let explain _i' = "(* lexeme: allow-letter in a string *)"|};
  [%expect
    {|
    1:61 single-letter y
    2:11 single-letter z
    3:13 single-letter _i'
    |}]
;;

let%expect_test "a letter is judged wherever a pattern binds it" =
  show
    {|let run ~f = fun v -> let%bind (j, k) = f v in j
let store = let (x, _ as y) = (0, 0) in x + y
let fold items = for i = 0 to 1 do () done|};
  [%expect
    {|
    1:10 single-letter f
    1:18 single-letter v
    1:33 single-letter j
    1:36 single-letter k
    2:18 single-letter x
    2:26 single-letter y
    3:22 single-letter i
    |}]
;;

let%expect_test "an interface pairs a plural with its singular" =
  show
    ~path:"input.mli"
    {|val matrices_of : int -> int list
val matrix_of : int -> int
module type Separate = sig val indices_at : int -> int list end
module type Another = sig val index_at : int -> int end|};
  [%expect {| 1:5 plural-lift matrices_of |}]
;;

let%expect_test "an interface names its values by their types" =
  show
    ~path:"input.mli"
    {|val does_matrix : unit -> bool
val multiply : int
val x (* lexeme: allow-letter published coefficient *) : int|};
  [%expect {|
    1:5 function-name does_matrix
    2:5 variable-name multiply
    |}]
;;

let%expect_test "an implementation names its values by their types and its patterns" =
  show
    {|let does_matrix : int -> int = Library.reduce
let applies value = value
let apply (multiply, exactly) = multiply
let ( @@ ) left right = left right|};
  [%expect
    {|
    1:5 function-name does_matrix
    1:5 predicate-result does_matrix
    2:5 function-name applies
    3:12 variable-name multiply
    3:22 variable-name exactly
    |}]
;;

let%expect_test "a predicate may give an optional or a fallible Boolean, with a note" =
  show
    ~path:"decisions.mli"
    {|val is_ready : (int -> string) -> count:int -> bool
val has_entry : int -> bool option
val does_accept : int -> bool Or_error.t
val is_enabled : int -> (bool, Error.t) result
val apply : int -> bool option|};
  [%expect
    {|
    2:5 predicate-state has_entry
    3:5 predicate-state does_accept
    4:5 predicate-state is_enabled
    |}]
;;

let%expect_test "a predicate that gives anything else is an error" =
  show
    ~path:"decisions.mli"
    {|val is_zero : int -> int
val has_rows : int -> int option
val has_width : int -> bool list
val does_apply : (int -> bool) -> string|};
  [%expect
    {|
    1:5 predicate-result is_zero
    2:5 predicate-result has_rows
    3:5 predicate-result has_width
    4:5 predicate-result does_apply
    |}]
;;

let%expect_test "a written result annotation names a predicate's result" =
  show
    {|let is_ready value : bool option = Some value
let does_accept value : bool Or_error.t = Ok value
let has_value value : int = value|};
  [%expect
    {|
    1:5 predicate-state is_ready
    1:14 boolean-parameter value
    2:5 predicate-state does_accept
    3:5 predicate-result has_value
    |}]
;;

let%expect_test "a predicate is judged by its type through aliases" =
  show
    ~path:"decisions.mli"
    {|type decision = bool option
type operation = int -> decision
val is_ready : operation|};
  [%expect {| 3:5 predicate-state is_ready |}]
;;

let%expect_test "a predicate is judged by its type through shadowing" =
  show
    ~path:"decisions.mli"
    {|type bool = int
val has_rows : unit -> bool
module Bool : sig type t = string end
val is_empty : unit -> Bool.t|};
  [%expect
    {|
    1:6 type-name bool
    2:5 predicate-result has_rows
    4:5 predicate-result is_empty
    |}]
;;

let%expect_test "a local alias of a function is judged by its type" =
  show {|let apply value =
  let does_reduce = String.length in
  does_reduce value|};
  [%expect {| 2:7 predicate-result does_reduce |}]
;;

let%expect_test "a parameter that holds a function is named as one" =
  show
    {|let apply (multiply : int -> int) number = multiply number
let table = object val mutable x = 0 method is_empty = true end
let map transformation number = transformation (number + 1)|};
  [%expect {|
    2:32 single-letter x
    3:9 function-name transformation
    |}]
;;

let%expect_test "a function's name agrees with what it gives" =
  show
    ~path:"agreement.mli"
    {|val contains : int list -> int -> bool
val is_empty : int list -> bool
val entries_of : int -> unit
val of_int : int -> unit
val print : int -> unit
val size_of : int list -> int|};
  [%expect
    {|
    1:5 predicate-name contains
    3:5 command-name entries_of
    4:5 command-name of_int
    |}]
;;

let%expect_test "an inferred type names a function as a written one does" =
  show {|let matches left right = left = right
let report count = ignore (count : int)|};
  [%expect {| 1:5 predicate-name matches |}]
;;

let%expect_test "types and fields are nouns, and a field that holds a Boolean is a \
                 predicate"
  =
  show
    ~path:"shapes.mli"
    {|type t
type matrix = { rows : int; is_square : bool; ready : bool; transpose : int -> int }
type quickly
type x|};
  [%expect
    {|
    2:47 predicate-name ready
    3:6 type-name quickly
    4:6 single-letter x
    |}]
;;

let%expect_test "a constructor names data with a noun phrase" =
  show
    ~path:"shapes.mli"
    {|type verdict = Acceptance | Rejection | Upper_bound of int | Accepted | Quickly|};
  [%expect {|
    1:62 constructor-name Accepted
    1:73 constructor-name Quickly
    |}]
;;

(* A function of [lines] lines of code, with a blank line and a comment between each,
   which do not count. *)
let function_of_length lines =
  let steps =
    List.init (lines - 2) ~f:(fun _ -> "  ignore (0 : int);\n\n  (* a step *)\n")
  in
  "let apply () =\n" ^ String.concat steps ^ "  ()\n"
;;

let%expect_test "a function past each limit earns a note, a warning and then an error" =
  List.iter [ 10; 11; 20; 21; 30; 31 ] ~f:(fun lines -> print (function_of_length lines));
  [%expect
    {|
    input.ml:1:5: note[function-length] apply: this definition has 11 source lines of code, more than 10
    input.ml:1:5: note[function-length] apply: this definition has 20 source lines of code, more than 10
    input.ml:1:5: warning[function-length] apply: this definition has 21 source lines of code, more than 20
    input.ml:1:5: warning[function-length] apply: this definition has 30 source lines of code, more than 20
    input.ml:1:5: error[function-length] apply: this definition has 31 source lines of code, more than 30
    |}]
;;

let%expect_test "an inline test has five more lines at each limit" =
  let test_of_length lines =
    let steps = List.init (lines - 2) ~f:(fun _ -> "  ignore (0 : int);\n") in
    "let%expect_test \"example\" =\n" ^ String.concat steps ^ "  ()\n"
  in
  List.iter [ 15; 16 ] ~f:(fun lines -> show (test_of_length lines));
  [%expect {| 1:1 function-length "example" |}]
;;

let%expect_test "an exemption spares a name but not its length" =
  show {|let main () = ()|};
  show
    (String.substr_replace_first (function_of_length 11) ~pattern:"apply" ~with_:"main");
  [%expect {| 1:5 function-length main |}]
;;

let%expect_test "limits must rise from note to warning to error" =
  [ "((note 10) (warning 20) (error 30))"
  ; "((note 20) (warning 10) (error 30))"
  ; "((note 0) (warning 0) (error 0))"
  ]
  |> List.map ~f:(fun text ->
    Or_error.try_with (fun () -> Judgement.Limits.t_of_sexp (Sexp.of_string text)))
  |> List.map ~f:Result.is_ok
  |> [%sexp_of: bool list]
  |> print_s;
  [%expect {| (true false false) |}]
;;

let%expect_test "a function body's one Boolean decision is an if-then-else" =
  show
    {|let decide flag = match flag with true -> 1 | false -> 0
let choose flag = if flag then 1 else 0
let compare first second = if first then 1 else if second then 2 else 3|};
  [%expect
    {|
    1:12 boolean-parameter flag
    1:19 lone-boolean-match match
    1:19 boolean-match match
    2:12 boolean-parameter flag
    3:5 if-else compare
    3:13 boolean-parameter first
    3:19 boolean-parameter second
    |}]
;;

let%expect_test "a match on several Booleans is an error, and on one a note" =
  show
    {|let tabulate first second = match first, second with true, _ -> 1 | _, true -> 2 | _ -> 3
let pick flag value = match flag, value with true, Some _ -> 1 | _ -> 0
let count items = match items with [] -> 0 | _ :: _ -> 1|};
  [%expect
    {|
    1:14 boolean-parameter first
    1:20 boolean-parameter second
    1:29 boolean-tuple match
    2:10 boolean-parameter flag
    2:23 boolean-match match
    |}]
;;

let%expect_test "a guard after work that it does not use asks to come first" =
  show
    {|let total items limit =
  let sum = List.fold items ~init:0 ~f:( + ) in
  match limit with
  | None -> Error "no limit"
  | Some bound -> Ok (min sum bound)|};
  [%expect {| 3:3 late-guard match |}]
;;

let%expect_test "a guard that uses the work, or follows no work, is in its place" =
  show
    {|let scale items factor =
  let sum = List.fold items ~init:0 ~f:( + ) in
  if sum > factor then sum else factor
let check items =
  let double item = item * 2 in
  if List.is_empty items then Error "empty" else Ok (List.map items ~f:double)|};
  [%expect {| |}]
;;

let%expect_test "a Boolean match within another Boolean decision asks for a variant type" =
  show
    {|let classify (first, second) =
  match first with
  | true -> (match second with true -> 1 | false -> 2)
  | false -> 3
let label (flag, items) = if flag then (match items with [] -> 0 | _ -> 1) else 2|};
  [%expect
    {|
    2:3 boolean-match match
    3:13 boolean-match match
    3:13 nested-boolean match
    |}]
;;

let%expect_test "each rule on the names of values explains itself" =
  print
    {|let x = 0
let quickly = 0
let matrix () = 0
let contains () = true
let entries_of () = ()|};
  [%expect
    {|
    input.ml:1:5: error[single-letter] x: this name is a single letter, so use a longer name, or add a comment "lexeme: allow-letter" that gives a reason
    input.ml:2:5: error[variable-name] quickly: this name should be a noun phrase, such as matrix or start_of_line, because it names a thing
    input.ml:3:5: error[function-name] matrix: this name should be a verb phrase, a predicate, or a noun phrase with a preposition, such as entries_of, because it names a function
    input.ml:4:5: error[predicate-name] contains: this name should be a predicate, such as is_empty or has_entries, because the function returns a Boolean
    input.ml:5:5: error[command-name] entries_of: this name should be a verb phrase, because a function that returns nothing is a command (command-query separation)
    |}]
;;

let%expect_test "each rule on the names of types explains itself" =
  print {|type quickly = Quickly
type record = { quickly : int }|};
  [%expect
    {|
    input.ml:1:6: error[type-name] quickly: this name should be a noun phrase, such as matrix or start_of_line, because it names a thing
    input.ml:1:16: error[constructor-name] Quickly: this name should be a noun phrase, such as matrix or start_of_line, because it names a thing
    input.ml:2:17: error[field-name] quickly: this name should be a noun phrase, such as matrix or start_of_line, because it names a thing
    |}]
;;

let%expect_test "each rule on results explains itself" =
  print
    {|let is_ready () = Some true
let does_accept () : bool Or_error.t = Ok true
let is_zero () = 0|};
  [%expect
    {|
    input.ml:1:5: note[predicate-state] is_ready: Are you sure Some false is a meaningfully different state from None? If so, you can ignore this note.
    input.ml:2:5: note[predicate-state] does_accept: Are you sure Ok false is a meaningfully different state from Error? If so, you can ignore this note.
    input.ml:3:5: error[predicate-result] is_zero: this predicate should return a Boolean, but it returns int
    |}]
;;

let%expect_test "each rule on plurals and lengths explains itself" =
  print {|let entries_of items = items
let entry_of item = item|};
  print (function_of_length 11);
  [%expect
    {|
    input.ml:1:5: note[plural-lift] entries_of: check whether this maps entry_of over a collection (line 2)
    input.ml:1:5: note[function-length] apply: this definition has 11 source lines of code, more than 10
    |}]
;;

let%expect_test "each rule on Boolean matches explains itself" =
  print
    {|let decide flag = match flag with true -> 1 | false -> 0
let tabulate first second = match first, second with true, _ -> 1 | _, true -> 2 | _ -> 3|};
  [%expect
    {|
    input.ml:1:12: note[boolean-parameter] flag: this parameter is a Boolean, so could a variant type name its cases?
    input.ml:1:19: warning[lone-boolean-match] match: this match is the only Boolean condition of its function, so use if-then-else
    input.ml:1:19: note[boolean-match] match: could a variant type name the cases of this Boolean?
    input.ml:2:14: note[boolean-parameter] first: this parameter is a Boolean, so could a variant type name its cases?
    input.ml:2:20: note[boolean-parameter] second: this parameter is a Boolean, so could a variant type name its cases?
    input.ml:2:29: error[boolean-tuple] match: this match takes 2 Booleans at once, so parse, don't validate, with a variant type
    |}]
;;

let%expect_test "each rule on if-then-else and Boolean parameters explains itself" =
  print {|let compare first second = if first then 1 else if second then 2 else 3|};
  [%expect
    {|
    input.ml:1:5: warning[if-else] compare: this function has if-then-else on lines 1, 1, so use pattern matching for all but one
    input.ml:1:13: note[boolean-parameter] first: this parameter is a Boolean, so could a variant type name its cases?
    input.ml:1:19: note[boolean-parameter] second: this parameter is a Boolean, so could a variant type name its cases?
    |}]
;;

let%expect_test "each rule on guards and nesting explains itself" =
  print
    {|let total items limit = let sum = List.length items in match limit with None -> 0 | Some _ -> sum
let classify first second = match first with true -> (match second with true -> 1 | false -> 2) | false -> 3|};
  [%expect
    {|
    input.ml:1:56: note[late-guard] match: this guard clause comes after unrelated work, so could it come first?
    input.ml:2:14: note[boolean-parameter] first: this parameter is a Boolean, so could a variant type name its cases?
    input.ml:2:20: note[boolean-parameter] second: this parameter is a Boolean, so could a variant type name its cases?
    input.ml:2:29: note[boolean-match] match: could a variant type name the cases of this Boolean?
    input.ml:2:54: note[boolean-match] match: could a variant type name the cases of this Boolean?
    input.ml:2:54: warning[nested-boolean] match: this conditional on Booleans lies within another, so parse, don't validate, with a variant type
    |}]
;;

let%expect_test "a projection does no work before a guard" =
  show
    {|type settings = { limit : int }
let choose settings items =
  let limit = settings.limit in
  match items with [] -> 0 | _ :: _ -> limit|};
  [%expect {| |}]
;;

let%expect_test "a local function carries the work that it uses" =
  show
    {|let second items limit =
  let total = List.length items in
  let cap () = min total 10 in
  match limit with None -> cap () | Some bound -> min bound total|};
  [%expect {| |}]
;;

(* The project's own modules are out of sight here, so lexeme's check of its own source
   shows the access that this rule finds, and the session's test shows where the project
   ends. *)
let%expect_test "a field of a local record, or of an installed library's, is no access" =
  show
    {|type point = { across : int; down : int }
let across_of point = point.across
let size_of (statistics : Hashtbl.statistics) = statistics.num_bindings
let buckets_of { Hashtbl.num_buckets; _ } = num_buckets|};
  [%expect {|
    1:16 field-name across
    2:5 function-name across_of
    |}]
;;

let%expect_test "a let that binds a pipeline mixes two styles, but a pipeline may end \
                 the chain"
  =
  show
    {|let spell words =
  let spellings = words |> List.map ~f:String.lowercase in
  String.concat spellings
let join words =
  let separator = " " in
  words |> List.map ~f:String.lowercase |> String.concat ~sep:separator|};
  [%expect {|
    2:7 let-pipeline let
    2:19 one-stage-pipeline |>
    |}]
;;

let%expect_test "a let that reads no result of the binds before it could come before them"
  =
  show
    {|let total_of value =
  let%bind count = value in
  let total = count + 1 in
  let limit = 10 in
  return (total + limit)|};
  [%expect {| 4:7 late-let let |}]
;;

let%expect_test "a lone bind between plain lets" =
  show
    {|let total_of value =
  let start = value + 1 in
  let%bind count = lookup start in
  let total = count + start in
  return total
let count_of value =
  let start = value + 1 in
  let%bind count = lookup start in
  return count|};
  [%expect {| 3:12 lone-bind let%bind |}]
;;

let%expect_test "a function writes each of a monad's operations in one notation" =
  show
    {|let total_of value =
  let%bind count = value in
  lookup count >>= fun total -> return total
let count_of value =
  let%map.Option count = value in
  count >>| succ
let length_of value = value >>= lookup >>| String.length|};
  [%expect
    {|
    1:5 mixed-notation total_of
    3:20 eta-expansion fun
    4:5 mixed-notation count_of
    |}]
;;

let%expect_test "a pipeline of one stage could be a plain application" =
  show
    {|let spell words = words |> List.rev
let join words = words |> List.rev |> String.concat ""|};
  [%expect {| 1:19 one-stage-pipeline |> |}]
;;

let%expect_test "an if-then-else whose branches are Booleans is its condition" =
  show
    {|let check ready = if ready then true else false
let flip ready = if ready then false else true
let pick ready = if ready then 1 else 0|};
  [%expect
    {|
    1:5 predicate-name check
    1:11 boolean-parameter ready
    1:19 boolean-branches if
    2:5 predicate-name flip
    2:10 boolean-parameter ready
    2:18 boolean-branches if
    3:10 boolean-parameter ready
    |}]
;;

let%expect_test "a function that applies a function to its parameter is that function" =
  show
    {|let spell words = List.map (fun word -> String.lowercase_ascii word) words
let pair words = List.map (fun word -> word, word) words
let mark words = List.map (fun word -> word ^ word) words|};
  [%expect {| 1:28 eta-expansion fun |}]
;;

let%expect_test "a function with one case could be fun" =
  show
    {|let head_of = function (head, _) -> head
let size = function [] -> 0 | _ :: _ -> 1|};
  [%expect {| 1:15 one-case-function function |}]
;;

let%expect_test "ignore gives the type of what it ignores" =
  show {|let run action = ignore (action ())
let start action = ignore (action () : int)|};
  [%expect {| 1:18 untyped-ignore ignore |}]
;;

let%expect_test "a length compared with zero could be is_empty" =
  show
    {|let is_blank words = List.length words = 0
let has_text text = 0 < String.length text
let is_long words = List.length words > 3|};
  [%expect {|
    1:22 length-comparison =
    2:21 length-comparison <
    |}]
;;

let%expect_test "a named function's Boolean parameter could be a variant" =
  show
    {|let paint ~glow colour = if glow then colour else 0
let shade colour = List.filter (fun mark -> mark) colour|};
  [%expect {| 1:12 boolean-parameter glow |}]
;;

let%expect_test "an anonymous function within two others" =
  show
    {|let count rows =
  List.map (fun row -> 1 + List.length (List.filter (fun cells -> not (List.exists (fun cell -> cell > 0) cells)) row)) rows
let shift rows =
  List.map (fun row -> List.length (List.map (fun cells -> let increase digits = List.map (fun digit -> digit + 1) digits in increase cells) row)) rows|};
  [%expect {| 2:84 nested-function fun |}]
;;

let%expect_test "each idiom of a function's shape explains itself" =
  print
    {|let total items = items |> List.length
let check ready = if ready then true else false
let spell words = List.map (fun word -> String.lowercase_ascii word) words
let head_of = function (head, _) -> head|};
  [%expect
    {|
    input.ml:1:19: note[one-stage-pipeline] |>: this pipeline has one stage, so could it be a plain application?
    input.ml:2:5: error[predicate-name] check: this name should be a predicate, such as is_empty or has_entries, because the function returns a Boolean
    input.ml:2:11: note[boolean-parameter] ready: this parameter is a Boolean, so could a variant type name its cases?
    input.ml:2:19: warning[boolean-branches] if: the branches of this if-then-else are true and false, so the expression is its condition, or the negation of it
    input.ml:3:28: note[eta-expansion] fun: this anonymous function applies a function to its parameter and does nothing else, so could it be that function itself?
    input.ml:4:15: note[one-case-function] function: this function has one case, so could fun take its pattern?
    |}]
;;

let%expect_test "each idiom of a function's values explains itself" =
  print
    {|let run action = ignore (action ())
let is_blank words = List.length words = 0
let paint ~glow colour = if glow then colour else 0
let count rows = List.map (fun row -> 1 + List.length (List.filter (fun cells -> not (List.exists (fun cell -> cell > 0) cells)) row)) rows|};
  [%expect
    {|
    input.ml:1:18: warning[untyped-ignore] ignore: this ignore has no type annotation, so a partial application could pass through it unseen; could it be ignore (value : type)?
    input.ml:2:22: note[length-comparison] =: this compares a length with zero, so could is_empty, which for a list stops at the first element, take its place?
    input.ml:3:12: note[boolean-parameter] glow: this parameter is a Boolean, so could a variant type name its cases?
    input.ml:4:99: note[nested-function] fun: this anonymous function is within two others, so could a let name one of them?
    |}]
;;
