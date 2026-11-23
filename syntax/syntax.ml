open! Core

module Program = struct
  type _ t =
    | Return : 'answer -> 'answer t
    | Declare : Binding.t * 'answer t -> 'answer t
    | Decide : Decision.t * 'answer t -> 'answer t
    | Enclose : 'body t * ('body -> 'answer t) -> 'answer t
    | Define : Binding.t * 'body t * ('body -> 'answer t) -> 'answer t

  let return answer = Return answer

  let rec bind : type answer result. answer t -> f:(answer -> result t) -> result t =
    fun program ~f:continuation ->
    let continue rest = bind rest ~f:continuation in
    match program with
    | Return answer -> continuation answer
    | Declare (binding, rest) -> Declare (binding, continue rest)
    | Decide (decision, rest) -> Decide (decision, continue rest)
    | Enclose (body, rest) -> Enclose (body, fun answer -> continue (rest answer))
    | Define (binding, body, rest) ->
      Define (binding, body, fun answer -> continue (rest answer))
  ;;

  let map = `Define_using_bind
end

include Program
include Monad.Make (Program)

let declare binding = Declare (binding, return ())
let decide decision = Decide (decision, return ())
let enclose body = Enclose (body, return)
let define binding body = Define (binding, body, return)

module Recorder = struct
  (* The steps of the innermost scope so far, in reverse. The enclosing scopes' steps wait
     on the stack of the traversal. *)
  type t = { mutable steps : unit Program.t list }

  let add recorder step = recorder.steps <- step :: recorder.steps
  let declare recorder binding = add recorder (declare binding)
  let decide recorder decision = add recorder (decide decision)

  let enclose_with recorder ~enclosure traversal =
    let outer_steps = recorder.steps in
    recorder.steps <- [];
    traversal ();
    let body = all_unit (List.rev recorder.steps) in
    recorder.steps <- enclosure body :: outer_steps
  ;;

  let enclose recorder traversal = enclose_with recorder ~enclosure:enclose traversal

  let define recorder binding traversal =
    enclose_with recorder ~enclosure:(define binding) traversal
  ;;
end

let record traversal =
  let recorder = { Recorder.steps = [] } in
  traversal recorder;
  all_unit (List.rev recorder.steps)
;;

type 'result algebra =
  { declare : Binding.t -> 'result
  ; decide : Decision.t -> 'result
  ; enclose : 'result list -> 'result
  ; define : Binding.t -> 'result list -> 'result
  }

let scope_of algebra inner_children =
  match inner_children with
  | [] -> []
  | _ :: _ -> [ algebra.enclose (List.rev inner_children) ]
;;

(* We gather a scope's children in reverse. *)
let rec children_of
  : type answer result. result algebra -> answer t -> result list -> answer * result list
  =
  fun algebra program children ->
  match program with
  | Return answer -> answer, children
  | Declare (binding, rest) ->
    children_of algebra rest (algebra.declare binding :: children)
  | Decide (decision, rest) ->
    children_of algebra rest (algebra.decide decision :: children)
  | Enclose (body, rest) ->
    children_after algebra body rest children ~closing:(scope_of algebra)
  | Define (binding, body, rest) ->
    let closing inner_children = [ algebra.define binding (List.rev inner_children) ] in
    children_after algebra body rest children ~closing

and children_after
  : type body answer result.
    result algebra
    -> body t
    -> (body -> answer t)
    -> result list
    -> closing:(result list -> result list)
    -> answer * result list
  =
  fun algebra body rest children ~closing ->
  let answer, inner_children = children_of algebra body [] in
  children_of algebra (rest answer) (closing inner_children @ children)
;;

let fold algebra program =
  let (), children = children_of algebra program [] in
  algebra.enclose (List.rev children)
;;
