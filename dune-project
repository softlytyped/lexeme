(lang dune 3.21)

(name lexeme)

(generate_opam_files true)

(implicit_transitive_deps false)

(source
 (github softlytyped/lexeme))

(license MIT)

(package
 (name lexeme)
 (synopsis "A linter for the names, branches and idioms of OCaml and C programs")
 (description
  "Lexeme checks that each name in an OCaml or C program is English for what it names. It also checks the length of each function, the way in which each function body branches, chains of let expressions, the use of record fields from other modules, and idioms that have a plainer form.")
 (depends
  (ocaml (>= 5.4))
  dune
  core
  core_unix
  (merlin-lib (= 5.6.1-504+ox))
  ppx_jane
  uucp
  yojson))
