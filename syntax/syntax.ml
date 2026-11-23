open! Core

(* A scoped operation opens a lexical block or a function's body. *)
module Scope = struct
  type t =
    | Block
    | Function_body of Binding.t
end

module Program = struct
  type 'answer t =
    | Return of 'answer
    | Declaration of Binding.t * 'answer t
    | Decision of Decision.t * 'answer t
    | Scope of Scope.t * unit t * 'answer t

  let return answer = Return answer

  let rec bind program ~f:proceed =
    let resume rest = bind rest ~f:proceed in
    match program with
    | Return answer -> proceed answer
    | Declaration (binding, rest) -> Declaration (binding, resume rest)
    | Decision (decision, rest) -> Decision (decision, resume rest)
    | Scope (scope, body, rest) -> Scope (scope, body, resume rest)
  ;;

  let map = `Define_using_bind
end

include Program
include Monad.Make (Program)

let declare binding = Declaration (binding, return ())
let decide decision = Decision (decision, return ())
let enclose body = Scope (Block, body, return ())
let define binding body = Scope (Function_body binding, body, return ())

module Recorder = struct
  (* The steps of the innermost scope so far, in reverse. The enclosing scopes' steps wait
     on the stack of the traversal. *)
  type t = { mutable steps : unit Program.t list }

  let add recorder step = recorder.steps <- step :: recorder.steps
  let declare recorder binding = add recorder (declare binding)
  let decide recorder decision = add recorder (decide decision)

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

(* We gather a scope's children in reverse. *)
let rec children_of algebra program children =
  match program with
  | Return () -> children
  | Declaration (binding, rest) ->
    children_of algebra rest (algebra.declare binding :: children)
  | Decision (decision, rest) ->
    children_of algebra rest (algebra.decide decision :: children)
  | Scope (scope, body, rest) ->
    let inner_children = children_of algebra body [] in
    children_of algebra rest (close algebra scope inner_children @ children)
;;

let fold algebra program = algebra.enclose (List.rev (children_of algebra program []))
