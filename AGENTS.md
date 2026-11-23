# Lexeme

Lexeme is a linter for the names and definitions of OCaml and C programs. A reader turns a
source file into a program of a free monad whose operations declare a name, mark a
conditional, and open a block or a function body. One fold judges that program, and each
rule is a handler in the fold's algebra.

## Code

Write clean, obviously correct, unfancy but categorically principled functional code.

- A new rule is a new handler. Where a rule needs more of the source than the operations
  give, add an operation to the signature, its case to the fold, and its handlers.
- Each concept has one module and one interface. A dune file lists exactly the libraries
  that its module reads.
- Errors are values: anything that a caller can cause returns `Or_error.t`.
- Write examples as expect tests and laws as property tests. Break each part on purpose
  and confirm that a test fails.

## Lexeme checks itself

Run lexeme on this directory before each commit, after a build so that Merlin sees the
current types. A change leaves no finding: no error, no warning and no note.

```sh
dune build @runtest
dune exec lexeme
```

## Writing

The README, comments and messages use only phrases that Wikipedia already uses, in the
sense that it uses them: noun phrase, predicate, command-query separation, guard clause,
source lines of code. A new idea is said in such phrases, never in a new term.
