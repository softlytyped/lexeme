open! Core

(* A scoped operation opens a lexical block or a function's body. *)
module Scope = struct
  type t =
    | Block
    | Function_body of Binding.t
end

(* The operations that answer [unit] alone, which a program performs one after another. *)
module Operation = struct
  type t =
    | Declaration of Binding.t
    | Decision of Decision.t
    | Access of Access.t
    | Chain of Chain.t
    | Notation of Notation.t
end

module Program = struct
  type 'answer t =
    | Return of 'answer
    | Step of Operation.t * 'answer t
    | Scope of Scope.t * unit t * 'answer t

  let return answer = Return answer

  let rec bind program ~f:continue_with =
    match program with
    | Return answer -> continue_with answer
    | Step (operation, rest) -> Step (operation, bind rest ~f:continue_with)
    | Scope (scope, body, rest) -> Scope (scope, body, bind rest ~f:continue_with)
  ;;

  let map = `Define_using_bind
end

include Program
include Monad.Make (Program)

let perform operation = Step (operation, return ())
let declare binding = perform (Declaration binding)
let decide decision = perform (Decision decision)
let access field_access = perform (Access field_access)
let chain links = perform (Chain links)
let sequence notation = perform (Notation notation)
let enclose body = Scope (Block, body, return ())
let define binding body = Scope (Function_body binding, body, return ())

module Recorder = struct
  (* The steps of the innermost scope so far, in reverse. The enclosing scopes' steps wait
     on the stack of the traversal. *)
  type t = { mutable steps : unit Program.t list }

  let add recorder step = recorder.steps <- step :: recorder.steps
  let declare recorder binding = add recorder (declare binding)
  let decide recorder decision = add recorder (decide decision)
  let access recorder field_access = add recorder (access field_access)
  let chain recorder links = add recorder (chain links)
  let sequence recorder notation = add recorder (sequence notation)

  let nest recorder scope traverse =
    let outer_steps = recorder.steps in
    recorder.steps <- [];
    traverse ();
    let body = all_unit (List.rev recorder.steps) in
    recorder.steps <- Scope (scope, body, return ()) :: outer_steps
  ;;

  let enclose recorder traverse = nest recorder Block traverse
  let define recorder binding traverse = nest recorder (Function_body binding) traverse
end

let record traverse =
  let recorder = { Recorder.steps = [] } in
  traverse recorder;
  all_unit (List.rev recorder.steps)
;;

type 'result algebra =
  { declare : Binding.t -> 'result
  ; decide : Decision.t -> 'result
  ; access : Access.t -> 'result
  ; chain : Chain.t -> 'result
  ; sequence : Notation.t -> 'result
  ; enclose : 'result list -> 'result
  ; define : Binding.t -> 'result list -> 'result
  }

(* A block that holds nothing is left out. *)
let close algebra (scope : Scope.t) inner_children =
  match scope, inner_children with
  | Block, [] -> []
  | Block, _ :: _ -> [ algebra.enclose (List.rev inner_children) ]
  | Function_body binding, _ -> [ algebra.define binding (List.rev inner_children) ]
;;

let interpret algebra : Operation.t -> _ = function
  | Declaration binding -> algebra.declare binding
  | Decision decision -> algebra.decide decision
  | Access field_access -> algebra.access field_access
  | Chain links -> algebra.chain links
  | Notation notation -> algebra.sequence notation
;;

(* We gather a scope's children in reverse. *)
let rec children_of algebra program children =
  match program with
  | Return () -> children
  | Step (operation, rest) ->
    children_of algebra rest (interpret algebra operation :: children)
  | Scope (scope, body, rest) ->
    let inner_children = children_of algebra body [] in
    children_of algebra rest (close algebra scope inner_children @ children)
;;

let fold algebra program = algebra.enclose (List.rev (children_of algebra program []))
