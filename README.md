# Lexeme

Lexeme is a linter for OCaml and C. It reads each name where a program binds it and asks
whether the name reads as English for what it names: a noun phrase for a value, a verb
phrase for a function that acts, and a predicate for a function that answers a yes-no
question. It also checks the length of each function and how its body branches.

```console
$ lexeme
src/matrix.ml:12:5: error[predicate-name] contains: expected a predicate, such as is_empty or has_entries, for a Boolean
src/matrix.ml:40:5: warning[function-length] multiply: 23 source lines of code, more than 20
src/matrix.ml:52:3: note[boolean-match] match: could a variant type name the cases of this Boolean?
```

## Install

Lexeme reads OCaml through Merlin and C through Clang. It builds with dune against the
Merlin library for OxCaml 5.4, and it needs `clang` on the path to read C.

```sh
opam install . --deps-only
dune build
```

Build the project that you check before you check it, so that Merlin finds its
dependencies, its ppx configuration and its types.

## Usage

```sh
lexeme                       # every OCaml and C file here that Git tracks or does not ignore
lexeme src include/vector.h  # a directory, through Git, or a single file
lexeme -config lint.sexp     # settings from a file other than the nearest .lexeme
lexeme -I include -D NDEBUG  # include directories and macro definitions for C
```

Each finding takes one line, `path:line:column: severity[code] subject: message`. A note
asks a question, a warning asks for a change, and an error fails the run. The exit status
is 0 when no finding is an error, 1 when one is, and 2 when a file cannot be read or
parsed.

## Names

Lexeme splits a name into words at its underscores, and reads each word with the parts of
speech that WordNet gives it, along with the participles of its verbs and the comparatives
and superlatives of its adjectives. The types come from the compiler.

| What the name binds | Expected | Examples |
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

A function that returns unit is a command, in the sense of command-query separation, so a
verb names it. A verb phrase may end in a prepositional phrase. A constructor names data,
so a noun phrase names it. A parameter that holds a function is named as a function, and a type alias
of `bool` is a Boolean. A predicate that returns `bool option` or `bool Or_error.t` earns a
note, which asks whether `Some false` differs from `None`, or `Ok false` from `Error`.

A single letter is an error, unless a comment that follows it on the same line gives a
reason. The type name `t` follows the convention of OCaml modules.

```ocaml
let square x (* lexeme: allow-letter coordinate in the published formula *) = x * x
```

When a scope defines both `entries_of` and `entry_of`, a note asks whether the plural
maps the singular over a collection. The head of the subject decides the plural, so
`matrix_of_indices` has no singular.

## Conditionals

Lexeme reads how each OCaml function body branches.

| Code | Severity | What it finds |
| --- | --- | --- |
| `boolean-match` | note | a match on one Boolean, which a variant type might name |
| `lone-boolean-match` | warning | a function body's only Boolean condition, written as a match and not an if-then-else |
| `if-else` | warning | more than one if-then-else in a function body |
| `boolean-tuple` | error | a match on two or more Booleans at once |
| `nested-boolean` | warning | a match on a Boolean nested in another Boolean condition |
| `late-guard` | note | a guard clause that comes after work which neither its condition nor one of its branches uses |

The last three follow the principle "parse, don't validate": a variant type that names
the cases replaces the Booleans.

## Lengths

A function with more than 10 source lines of code earns a note, more than 20 a warning
and more than 30 an error. Blank lines and lines that hold only comments do not count. An
inline test, such as `let%expect_test`, has five more lines at each limit.

## Settings

The nearest `.lexeme` file, in the working directory or one of its parents, holds a
project's settings as an S-expression. Every field is optional.

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
| `vocabulary` | words that the project adds to the English lexicon |
| `function_lines` | the limits on source lines of code |
| `exemptions` | names that are not judged as names, `main` and `equal` by default |
| `directories` | include directories for C, relative to the file |
| `definitions` | macro definitions for C |

## How it works

A reader turns a source file into a program of a free monad over four operations: declare
a name, mark a conditional, open a block, and open a function body. Merlin gives the OCaml
parse tree, its comments and the types that the compiler infers, and Clang gives the C
syntax tree as JSON. One fold judges the program, and its algebra has a handler for each
operation, so a new rule is a new handler.

The English lexicon is Princeton WordNet 3.0, restricted to single words and the
inflections of nouns. The extraction script and the WordNet license are in
`lexicon/english`. To rebuild the table from the WordNet 3.0 archive that NLTK
distributes, run

```sh
bash lexicon/english/extract.sh /path/to/wordnet.zip > lexicon/english/wordnet.words
```

## License

Lexeme is free software under the MIT License, which allows any use, commercial or not,
so long as the license notice stays with the software. The WordNet table keeps the
WordNet 3.0 license, which is in `lexicon/english`.
