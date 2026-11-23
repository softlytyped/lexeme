open! Core
open Ocaml_parsing
open Parsetree
module Codomain = Binding.Codomain
module Role = Binding.Role
module Shape = Session.Shape

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

let shape_of_annotation annotation =
  let codomain = lazy (codomain_of annotation) in
  if is_arrow annotation then Shape.Function codomain else Value codomain
;;

let unknown = Lazy.from_val Codomain.Unknown

let codomain_of_return = function
  | Pconstraint annotation | Pcoerce (_, annotation) -> lazy (codomain_of annotation)
;;

let rec shape_of_expression expression =
  match expression.pexp_desc with
  | Pexp_function (_, signature, _) ->
    let returned = Option.map signature.ret_type_constraint ~f:codomain_of_return in
    Shape.Function (Option.value returned ~default:unknown)
  | Pexp_constraint (_, Some annotation, _) | Pexp_coerce (_, _, annotation) ->
    shape_of_annotation annotation
  | Pexp_constraint (body, None, _)
  | Pexp_newtype (_, _, body)
  | Pexp_let (_, _, _, body)
  | Pexp_letop { body; _ }
  | Pexp_open (_, body)
  | Pexp_sequence (_, body) -> shape_of_expression body
  | _ -> Value unknown
;;

let prefer inference annotation =
  lazy
    (match force inference with
     | Codomain.Unknown -> force annotation
     | codomain -> codomain)
;;

let merge ~inference ~annotation =
  match inference, annotation with
  | Some (Shape.Function inferred), Shape.Function written ->
    Shape.Function (prefer inferred written)
  | Some (Value inferred), Value written -> Value (prefer inferred written)
  | Some (Function _ as shape), Value _ -> shape
  | Some (Value _), (Function _ as shape) | None, shape -> shape
;;

let shape_at session ?extent annotation (name : string Location.loc) =
  let inference =
    List.find_map (name.loc :: Option.to_list extent) ~f:(fun location ->
      Session.shape_at session (Session.extent_of location))
  in
  merge ~inference ~annotation
;;

let role_of_value = function
  | Shape.Value _ -> Role.Variable
  | Function codomain -> Function codomain
;;

let written_shape_of_definition binding =
  match binding.pvb_constraint, binding.pvb_pat.ppat_desc with
  | Some (Pvc_constraint { typ = annotation; _ }), _
  | Some (Pvc_coercion { coercion = annotation; _ }), _
  | None, Ppat_constraint (_, Some annotation, _) -> shape_of_annotation annotation
  | None, _ -> shape_of_expression binding.pvb_expr
;;

let role_of_definition session binding name =
  let annotation = written_shape_of_definition binding in
  role_of_value (shape_at session ~extent:binding.pvb_pat.ppat_loc annotation name)
;;

let role_of_description session annotation name =
  role_of_value (shape_at session (shape_of_annotation annotation) name)
;;

let role_of_pattern session name = role_of_value (shape_at session (Value unknown) name)

let role_of_label session annotation name =
  match shape_at session (shape_of_annotation annotation) name with
  | Value codomain -> Role.Field codomain
  | Function codomain -> Function codomain
;;

let role_of_method ?annotation ?extent session name =
  let written =
    Option.value_map annotation ~default:(Shape.Function unknown) ~f:shape_of_annotation
  in
  match shape_at session ?extent written name with
  | Value codomain | Function codomain -> Role.Function codomain
;;
