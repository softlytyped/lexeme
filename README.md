# Lexeme

Lexeme is a linter for programs in OCaml and C. It checks that each name in a program is
English for what it names: a noun phrase for a value, a verb phrase for a function that
acts, and a predicate for a function that returns a Boolean. It also checks the length of
each function, the way in which each function body branches, the chains of `let`
expressions in OCaml, the use of record fields from other modules, and small idioms that
have a plainer form.

Lexeme parses OCaml with Merlin, which also supplies the types that the compiler infers,
and C with Clang. Its rules are handlers of a single fold over a program, so a rule about
names applies to both languages.

```console
$ lexeme src/matrix.ml
src/matrix.ml:1:5: error[predicate-name] contains: this name should be a predicate, such as is_empty or has_entries, because the function returns a Boolean
src/matrix.ml:1:41: note[eta-expansion] fun: this anonymous function applies a function to its parameter and does nothing else, so could it be that function itself?
src/matrix.ml:2:24: warning[boolean-branches] if: the branches of this if-then-else are true and false, so the expression is its condition, or the negation of it
src/matrix.ml:2:27: note[length-comparison] =: this compares a length with zero, so could is_empty, which for a list stops at the first element, take its place?
src/matrix.ml:3:22: note[one-stage-pipeline] |>: this pipeline has one stage, so could it be a plain application?
```

## Installation

Lexeme is built with dune against the Merlin library for OxCaml 5.4. Checking C also
requires `clang` on the path.

```sh
opam install . --deps-only
dune build
```

Merlin finds a project's dependencies, preprocessors and types in its build directory, so
a project is built before lexeme checks it.

## Usage

```sh
lexeme                       # every OCaml and C file that Git tracks or does not ignore
lexeme src include/vector.h  # a directory, through Git, or a single file
lexeme -config lint.sexp     # settings from a file other than the nearest .lexeme
lexeme -I include -D NDEBUG  # include directories and macro definitions for C
```

Each finding is one line of the form `path:line:column: severity[code] subject: message`.
A finding has one of three severities. A note is a question for the writer, a warning is
a request for a change, and an error fails the run. The exit status is 0 when no finding
is an error, 1 when at least one is, and 2 when a file cannot be parsed.

## Checks

### Names

Lexeme splits a name into words at its underscores. Each word gets its parts of speech
from WordNet, together with the plurals of its nouns, the participles of its verbs, and
the comparative and superlative forms of its adjectives. The type of each binding comes
from the compiler.

| Binding | Expected name | Examples |
| --- | --- | --- |
| A value | a noun phrase | `matrix`, `register_bits`, `start_of_line` |
| A function | a verb phrase | `apply`, `fully_contract`, `test_and_set`, `drain_into` |
| | a noun phrase with a preposition | `entries_of`, `entry_at_index`, `of_int` |
| | a predicate | `is_empty`, `has_entries`, `does_accept` |
| A function that returns a Boolean | a predicate | `is_sorted` |
| A function that returns unit or void | a verb phrase | `print`, `flush` |
| A record field or a struct member | a noun phrase, or a predicate for a Boolean | `count`, `is_open` |
| A type | a noun phrase | `matrix`, `t` |
| A constructor | a noun phrase | `Warning`, `Acceptance` |

A verb phrase is a verb, or verbs joined by a conjunction, and it may end in a
prepositional phrase. A function that returns unit is a command in the sense of
command-query separation, so its name is a verb phrase. A constructor names data, so its
name is a noun phrase. A parameter whose type is a function is named as a function, and a
type alias of `bool` counts as a Boolean. A predicate that returns `bool option` or
`bool Or_error.t` gets a note, because `Some false` may or may not differ from `None`,
and `Ok false` from `Error`.

A name of a single letter is an error, unless a comment that follows it on the same line
gives a reason. The type name `t` is the convention for a module's main type, and is
allowed.

```ocaml
let square x (* lexeme: allow-letter coordinate in the published formula *) = x * x
```

When one scope defines both `entries_of` and `entry_of`, the pair gets a note, because
the plural may map the singular over a collection. The plural of a name is the plural of
its head, so `matrix_of_indices` has no singular.

### Conditionals

These rules apply to OCaml function bodies.

| Code | Severity | Finding |
| --- | --- | --- |
| `boolean-match` | note | a match on one Boolean, whose cases a variant type could name |
| `lone-boolean-match` | warning | the only Boolean condition of a function body, written as a match instead of an if-then-else |
| `if-else` | warning | more than one if-then-else in one function body |
| `boolean-tuple` | error | a match on two or more Booleans at once |
| `nested-boolean` | warning | a match on a Boolean within another Boolean condition |
| `late-guard` | note | a guard clause after work that neither its condition nor its branches use |

The last three rules follow the principle "parse, don't validate", under which a variant
type with a constructor for each case replaces the Booleans.

### Chains of let expressions

Lexeme follows each chain of `let ... in` expressions in OCaml, together with the binds
and maps of a monad, whether they are written as `let%bind`, `let%map`, `let*` and
`let+`, or as `>>=` and `>>|`.

| Code | Severity | Finding |
| --- | --- | --- |
| `let-pipeline` | note | a `let` that binds the result of a pipeline, where the pipeline could end the chain |
| `late-let` | note | a plain `let` after a bind, which uses no value from the binds before it |
| `lone-bind` | note | the only bind of a chain, with plain `let` expressions on both sides of it |
| `mixed-notation` | warning | a function that writes bind, or map, both as an infix operator and as a `let` |

### Record fields

Code that uses a record field from another module of the same project depends on the way
in which that module stores its data. Lexeme gives a warning, `field-access`, on each
projection, record pattern or record update of such a field. Building a record in full is
not an access, since all of its fields are written out, as the arguments of a constructor
are.

### Idioms

An idiom here is a small form of an OCaml expression that a plainer form can replace.

| Code | Severity | Form | Plainer form |
| --- | --- | --- | --- |
| `one-stage-pipeline` | note | `x \|> f` | `f x` |
| `boolean-branches` | warning | `if ready then true else false` | `ready` |
| `eta-expansion` | note | `fun x -> f x`, as an anonymous function | `f` |
| `one-case-function` | note | `function (x, _) -> x` | `fun (x, _) -> x` |
| `untyped-ignore` | warning | `ignore (f x)` | `ignore (f x : int)` |
| `length-comparison` | note | `List.length items = 0` | `List.is_empty items` |
| `boolean-parameter` | note | `let draw ~filled shape = ...` | a variant type for the two cases |
| `nested-function` | note | an anonymous function within two others | a named function |

An `ignore` without a type annotation also accepts a partially applied function, which is
usually a mistake. For a list, a comparison of its length with zero counts every
element, while `is_empty` stops at the first. The rule on Boolean parameters applies to named
functions only.

### Lengths

A function with more than 10 source lines of code gets a note, one with more than 20 a
warning, and one with more than 30 an error. Blank lines and lines that contain only
comments do not count. An inline test, such as `let%expect_test`, has five more lines at
each limit.

## Settings

Settings come from the nearest `.lexeme` file in the working directory or one of its
parents. The file contains an S-expression, and every field is optional.

```scheme
((vocabulary
  ((nouns (adjoint hamiltonian))
   (verbs (tensor))
   (adjectives (monoidal))
   (adverbs (pointwise))
   (plurals ((qubits qubit)))))
 (function_lines ((note 10) (warning 20) (error 30) (test_allowance 5)))
 (exemptions (main equal))
 (directories (include))
 (definitions (NDEBUG)))
```

| Field | Meaning |
| --- | --- |
| `vocabulary` | words that a project adds to the English lexicon |
| `function_lines` | the limits on source lines of code |
| `exemptions` | names that are exempt from the rules on names, `main` and `equal` by default |
| `directories` | include directories for C, relative to the file |
| `definitions` | macro definitions for C |

## Design

Each front end turns a source file into a program of a free monad over eight operations.
Six of them are simple: a program declares a name, marks a conditional, uses a record
field of another module, links a chain of local definitions, uses a monad's bind or map,
and expresses an idiom. The other two are scoped: one opens a block, and one opens a
function body. A single fold interprets the program, and its algebra has a handler for
each operation. A new rule is therefore a new handler, and a rule that needs more of the
source adds an operation.

The OCaml front end takes the parse tree, the comments and the inferred types from
Merlin. The C front end takes the syntax tree that Clang emits as JSON.

The English lexicon is Princeton WordNet 3.0, restricted to single words and the
inflections of nouns. The extraction script and the WordNet license are in
`lexicon/english`. The table is rebuilt from the WordNet 3.0 archive that NLTK
distributes with the following command:

```sh
bash lexicon/english/extract.sh /path/to/wordnet.zip > lexicon/english/wordnet.words
```

## License

Lexeme is free software under the MIT License, which allows any use, commercial or not,
as long as the license notice stays with the software. The WordNet table in
`lexicon/english` is under the WordNet 3.0 license, which is in the same directory.
