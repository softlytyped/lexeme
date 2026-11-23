(lang dune 3.21)

(name lexeme)

(generate_opam_files true)

(implicit_transitive_deps false)

(source
 (github softlytyped/lexeme))

(license MIT)

(package
 (name lexeme)
 (synopsis "A linter for the names and definitions of OCaml and C programs")
 (description
  "Lexeme reads each name where a program binds it and asks whether the name reads as English for what it names, and checks the length of each function and how its body branches.")
 (depends
  (ocaml (>= 5.4))
  dune
  core
  core_unix
  (merlin-lib (= 5.6.1-504+ox))
  ppx_jane
  uucp
  yojson))
