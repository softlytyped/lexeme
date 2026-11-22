# Lexeme

Lexeme checks the grammar of a name where the program binds it. It reads OCaml
implementations and interfaces with the OCaml parser and reads C sources and headers
with Clang.

From the Blackglass checkout, use its opam switch.

```sh
opam exec --switch=. -- dune exec lexeme
opam exec --switch=. -- dune exec lexeme -- bgir/dense/register.mli
opam exec --switch=. -- dune runtest lexeme
```

With no arguments Lexeme checks the tracked and unignored new source files in the
current Git directory. A directory argument selects that part of its repository.
Explicit file arguments also work outside Git. The exit status is 0 for a clean run
or notes alone, 1 for naming or predicate errors, and 2 for an input or tool failure.

## Names

| Binding | Examples |
| --- | --- |
| Variable with a noun head | `matrix`, `register_bits`, `strictly_positive_integer` |
| Root verb with optional adverbs | `contract`, `fully_contract`, `contract_exactly` |
| Verbs joined by a conjunction | `test_and_set`, `load_or_store` |
| Noun and preposition | `entries_of`, `entry_at`, `sparse_matrix_of_bits` |
| Preposition and noun | `of_int`, `from_entries` |
| Predicate | `is_zero`, `is_at_end`, `has_entries`, `does_accept` |

The grammar reads underscore-separated words. A word can have several parts of speech.
For example, `fold` can name an action or a noun. An auxiliary determines its complement.
`has_` takes a noun phrase and `does_` takes a root verb phrase.

A deliberate single-letter exception goes immediately after the identifier, in a
same-line block comment with a reason. It applies to that occurrence alone. Leading
underscores and trailing OCaml primes still count as a single letter in `_i'`.

```ocaml
let square x (* lexeme: allow-letter coordinate in the published formula *) = x * x
```

```c
int square(int x /* lexeme: allow-letter coordinate in the published formula */) {
  return x * x;
}
```

When a scope defines both `sparse_matrices_at` and `sparse_matrix_at`, Lexeme asks
whether the plural applies the singular element by element. The head noun is
`matrices`. In `matrix_of_indices`, the plural `indices` belongs to the complement.
The note names the singular declaration and its line. Each source file has its own
root scope.

Predicates return a Boolean. Lexeme also accepts `bool option` and `bool Or_error.t`
and asks whether `Some false` differs meaningfully from `None`, or `Ok false` from
`Error`. That note can be ignored when both states have meaning. A different or
undetermined predicate result is an error.

## The syntax fold

Both readers interpret the same two operations. `bind` receives a declaration and
its source position. `enclose` receives the interpretations of a lexical scope's
children. The naming interpreter collects the immediate declarations at each scope
so it can see a singular sibling declared later and respect rebinding. A new reader
supplies a fold into this algebra. The grammar and diagnostics are shared.

OCaml function definitions and signatures carry their declared types where available.
Merlin supplies inferred types from the current source and expands aliases at a
predicate's binding when needed. Build the OCaml project to make its dependencies and
ppx configuration available to Merlin. Clang checks the active preprocessor
configuration. Included declarations are checked when their own file is selected.
Blackglass's C workload uses the Tree-sitter header in its staged corpus.

## Vocabulary

The English dictionary is compiled into Lexeme. It contains single-word noun,
root verb, adjective and adverb readings and irregular noun inflections from
Princeton WordNet 3.0. The extraction script and the distribution's license are in
`lexicon/english`. To reproduce the table, run
`bash lexicon/english/extract.sh /path/to/wordnet.zip > lexicon/english/wordnet.words`
from the submodule root with the WordNet 3.0 archive distributed by NLTK.

A `.lexeme` in the working directory adds domain readings. All fields are optional.
Include directories are relative to the configuration file. Use `-config FILE` to
select a configuration, `-I DIR` for an additional C include directory and
`-D MACRO` for a preprocessor definition.

```scheme
((vocabulary
  ((nouns (adjoint hamiltonian))
   (verbs (tensor))
   (adjectives (monoidal))
   (adverbs (pointwise))
   (plurals ((qubits qubit)))))
 (directories (include)))
```
