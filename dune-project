(lang dune 3.21)

(name lexeme)

(generate_opam_files true)

(implicit_transitive_deps false)

(package
 (name lexeme)
 (synopsis "Check the grammar of names at their binding sites")
 (depends
  (ocaml (>= 5.4))
  dune
  core
  core_unix
  merlin
  ppx_jane
  uucp
  yojson))
