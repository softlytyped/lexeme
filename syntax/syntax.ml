open! Core

module Program = struct
  type _ t =
    | Return : 'answer -> 'answer t
    | Declare : Binding.t * 'answer t -> 'answer t
    | Enclose : 'body t * ('body -> 'answer t) -> 'answer t

  let return answer = Return answer

  let rec bind : type answer result. answer t -> f:(answer -> result t) -> result t =
    fun program ~f:continuation ->
    match program with
    | Return answer -> continuation answer
    | Declare (binding, rest) -> Declare (binding, bind rest ~f:continuation)
    | Enclose (body, rest) ->
      Enclose (body, fun answer -> bind (rest answer) ~f:continuation)
  ;;

  let map = `Define_using_bind
end

include Program
include Monad.Make (Program)

let declare binding = Declare (binding, return ())
let enclose body = Enclose (body, return)

module Recorder = struct
  (* The steps of the innermost scope so far, in reverse. The enclosing scopes' steps wait
     on the stack of [enclose]. *)
  type t = { mutable steps : unit Program.t list }

  let program_of steps = all_unit (List.rev steps)
  let declare recorder binding = recorder.steps <- declare binding :: recorder.steps

  let enclose recorder traversal =
    let outer = recorder.steps in
    recorder.steps <- [];
    traversal ();
    recorder.steps <- enclose (program_of recorder.steps) :: outer
  ;;
end

let record traversal =
  let recorder = { Recorder.steps = [] } in
  traversal recorder;
  Recorder.program_of recorder.steps
;;

type 'result algebra =
  { declare : Binding.t -> 'result
  ; enclose : 'result list -> 'result
  }

let scope_of algebra children =
  match children with
  | [] -> []
  | _ :: _ -> [ algebra.enclose (List.rev children) ]
;;

(* We gather a scope's children in reverse, and leave out a scope that has none. *)
let rec children_of
  : type answer result. result algebra -> answer t -> result list -> answer * result list
  =
  fun algebra program children ->
  match program with
  | Return answer -> answer, children
  | Declare (binding, rest) ->
    children_of algebra rest (algebra.declare binding :: children)
  | Enclose (body, rest) ->
    let answer, inner = children_of algebra body [] in
    children_of algebra (rest answer) (scope_of algebra inner @ children)
;;

let fold algebra program =
  let (), children = children_of algebra program [] in
  algebra.enclose (List.rev children)
;;
