open! Core
open Ocaml_parsing
open Parsetree
module Codomain = Binding.Codomain
module Role = Binding.Role

let rec is_arrow annotation =
  match annotation.ptyp_desc with
  | Ptyp_arrow _ -> true
  | Ptyp_poly (_, body) -> is_arrow body
  | _ -> false
;;

let rec result_of annotation =
  match annotation.ptyp_desc with
  | Ptyp_poly (_, body) | Ptyp_arrow (_, _, body, _, _) -> result_of body
  | _ -> annotation
;;

let public_path_of path =
  match String.lsplit2 path ~on:'.' with
  | Some (("Core" | "Base" | "Stdlib"), name) -> name
  | _ -> path
;;

let constructor_of annotation =
  match annotation.ptyp_desc with
  | Ptyp_constr (path, arguments) ->
    public_path_of (Format.asprintf "%a" Pprintast.longident path.txt), arguments
  | _ -> "", []
;;

let is_constructor annotation spellings =
  match constructor_of annotation with
  | path, [] -> List.mem spellings path ~equal:String.equal
  | _, _ :: _ -> false
;;

let codomain_of annotation =
  let result = result_of annotation in
  let is_boolean value = is_constructor value [ "bool"; "Bool.t" ] in
  match constructor_of result with
  | ("bool" | "Bool.t"), [] -> Codomain.Boolean
  | ("unit" | "Unit.t"), [] -> Unit "unit"
  | ("option" | "Option.t"), [ value ] when is_boolean value -> Optional_boolean
  | "Or_error.t", [ value ] when is_boolean value -> Fallible_boolean
  | ("result" | "Result.t"), [ value; error ]
    when is_boolean value && is_constructor error [ "Error.t" ] -> Fallible_boolean
  | _ -> Other (Format.asprintf "%a" Pprintast.core_type result)
;;

let function_of annotation =
  Role.Function
    (lazy (Option.value_map annotation ~default:Codomain.Unknown ~f:codomain_of))
;;

let role_of_annotation annotation =
  match is_arrow annotation with
  | true -> function_of (Some annotation)
  | false -> Role.Variable
;;

let return_annotation_of = function
  | Pconstraint annotation | Pcoerce (_, annotation) -> annotation
;;

let rec role_of_expression expression =
  match expression.pexp_desc with
  | Pexp_function (_, signature, _) ->
    function_of (Option.map signature.ret_type_constraint ~f:return_annotation_of)
  | Pexp_constraint (_, Some annotation, _) | Pexp_coerce (_, _, annotation) ->
    role_of_annotation annotation
  | Pexp_constraint (body, None, _)
  | Pexp_newtype (_, _, body)
  | Pexp_let (_, _, _, body)
  | Pexp_letop { body; _ }
  | Pexp_open (_, body)
  | Pexp_sequence (_, body) -> role_of_expression body
  | _ -> Role.Variable
;;

let merge ~inferred ~written =
  match inferred, written with
  | Some (Role.Function inferred), Role.Function written ->
    Role.Function
      (lazy
        (match force inferred with
         | Codomain.Unknown -> force written
         | codomain -> codomain))
  | Some (Function _ as role), Variable -> role
  | Some Variable, _ | None, _ -> written
;;

let role_at session ?extent written (name : string Location.loc) =
  let inferred =
    List.find_map (name.loc :: Option.to_list extent) ~f:(fun location ->
      Session.role_at session (Session.extent_of location))
  in
  merge ~inferred ~written
;;

let written_role_of_definition binding =
  match binding.pvb_constraint, binding.pvb_pat.ppat_desc with
  | Some (Pvc_constraint { typ = annotation; _ }), _
  | Some (Pvc_coercion { coercion = annotation; _ }), _
  | None, Ppat_constraint (_, Some annotation, _) -> role_of_annotation annotation
  | None, _ -> role_of_expression binding.pvb_expr
;;

let role_of_definition session binding =
  role_at session ~extent:binding.pvb_pat.ppat_loc (written_role_of_definition binding)
;;

let role_of_type session annotation = role_at session (role_of_annotation annotation)

let role_of_method ?annotation ?extent session =
  role_at session ?extent (function_of annotation)
;;
