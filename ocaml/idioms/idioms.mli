(** The idioms of an OCaml source: small forms of expressions that plainer forms can
    replace. *)

open! Core
open Ocaml_parsing

(** [parameters_of definition] returns the patterns of a function definition's parameters,
    beneath any type annotation, or none when the definition is no function. *)
val parameters_of : Parsetree.expression -> Parsetree.pattern list

(** [forms_in syntax_tree] returns the idioms of a source by the extents of their
    expressions, each with the keyword or the operator that names it. An expression may be
    an idiom by its own shape, or by its surroundings: a pipeline has one stage when it is
    the operand of no pipeline, and an anonymous function is nested within two others of
    the same named function, or is an eta-expansion, which a named function may keep. *)
val forms_in
  :  Merlin_kernel.Mreader.parsetree
  -> (Idiom.Form.t * string) list Source.Extent.Map.t
