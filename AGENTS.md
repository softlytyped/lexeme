# Lexeme

Lexeme is a linter for the names and definitions of OCaml and C programs. Each front end
turns a source file into a program of a free monad. The operations of the monad declare a
name, mark a conditional, use a record field of another module, link a chain of local
definitions, use a monad's bind or map, express an idiom, and open a block or a function
body. One fold interprets the program, and each rule is a handler in the fold's algebra.

## Code

Lexeme's code is functional, clean, obviously correct and unfancy, and its structure
follows the principles of category theory.

- A new rule is a new handler. When a rule needs more of the source than the operations
  carry, the same change adds an operation to the signature, its case to the fold, and
  its handlers.
- Each concept has one module with one interface. Each dune file lists exactly the
  libraries that its module uses.
- Errors are values. Wherever a caller can cause a failure, the function returns
  `Or_error.t`.
- Examples are expect tests, and laws are property tests. Each part of lexeme is broken
  on purpose once, to confirm that a test fails.

## Self-check

Lexeme checks its own source before each commit, after a build, so that Merlin has the
current types. A change leaves no finding: no error, no warning and no note.

```sh
dune build @runtest
dune exec lexeme
```

## Writing

The README, the comments and the messages are written in the plain style of a Wikipedia
article. They use phrases that Wikipedia already uses, in the sense in which it uses
them, such as noun phrase, predicate, command-query separation, guard clause and source
lines of code. A new idea is expressed in such phrases, never in a new term.

A thing is never the subject of a verb for a person, such as read, know, want, hold or
sit. A compound such as record field is preferred to a phrase such as field of a record,
as long as the compound has at most two nouns.
