open! Core
open Ocaml_parsing
open Parsetree
open Binding_syntax

let rec is_function annotation : bool =
  match annotation.ptyp_desc with
  | Ptyp_arrow _ -> true
  | Ptyp_poly (_, body) -> is_function body
  | _ -> false
;;

let rec codomain_of annotation =
  match annotation.ptyp_desc with
  | Ptyp_poly (_, body) | Ptyp_arrow (_, _, body, _, _) -> codomain_of body
  | _ -> annotation
;;

let constructor_of annotation =
  match annotation.ptyp_desc with
  | Ptyp_constr (path, arguments) ->
    Format.asprintf "%a" Pprintast.longident path.txt, arguments
  | _ -> "", []
;;

let public_path_of path =
  match String.lsplit2 path ~on:'.' with
  | Some (("Core" | "Base" | "Stdlib"), name) -> name
  | _ -> path
;;

let is_boolean annotation =
  let path, arguments = constructor_of annotation in
  List.is_empty arguments
  && List.mem [ "bool"; "Bool.t" ] (public_path_of path) ~equal:String.equal
;;

let result_of annotation =
  let codomain = codomain_of annotation in
  let path, arguments = constructor_of codomain in
  match public_path_of path, arguments with
  | _, [] when is_boolean codomain -> Boolean
  | ("option" | "Option.t"), [ value ] when is_boolean value -> Optional_boolean
  | "Or_error.t", [ value ] when is_boolean value -> Fallible_boolean
  | ("result" | "Result.t"), [ value; error ]
    when is_boolean value
         && String.equal (public_path_of (fst (constructor_of error))) "Error.t" ->
    Fallible_boolean
  | _ -> Other (Format.asprintf "%a" Pprintast.core_type codomain)
;;

let function_of annotation =
  Function (lazy (Option.value_map annotation ~default:Unknown ~f:result_of))
;;

let role_of_annotation annotation =
  match is_function annotation with
  | true -> function_of (Some annotation)
  | false -> Variable
;;

let rec role_of_expression expression =
  match expression.pexp_desc with
  | Pexp_function (_, signature, _) ->
    function_of
      (Option.map signature.ret_type_constraint ~f:(function
         | Pconstraint annotation | Pcoerce (_, annotation) -> annotation))
  | Pexp_constraint (_, Some annotation, _) | Pexp_coerce (_, _, annotation) ->
    role_of_annotation annotation
  | Pexp_constraint (body, None, _)
  | Pexp_newtype (_, _, body)
  | Pexp_let (_, _, _, body)
  | Pexp_letop { body; _ }
  | Pexp_open (_, body)
  | Pexp_sequence (_, body) -> role_of_expression body
  | _ -> Variable
;;

let role_at types ?extent annotation (name : string Location.loc) =
  let inference =
    List.find_map (name.loc :: Option.to_list extent) ~f:(fun (location : Location.t) ->
      Inferred.role_at
        types
        ~start:location.loc_start.pos_cnum
        ~stop:location.loc_end.pos_cnum)
  in
  match inference, annotation with
  | Some (Function result), Function annotation ->
    Function
      (lazy
        (match force result with
         | Unknown -> force annotation
         | result -> result))
  | Some (Function _ as role), Variable -> role
  | Some Variable, _ | None, _ -> annotation
;;

let role_of_definition types binding =
  let annotation =
    match binding.pvb_constraint with
    | Some (Pvc_constraint { typ = annotation; _ })
    | Some (Pvc_coercion { coercion = annotation; _ }) -> role_of_annotation annotation
    | None ->
      (match binding.pvb_pat.ppat_desc with
       | Ppat_constraint (_, Some annotation, _) -> role_of_annotation annotation
       | _ -> role_of_expression binding.pvb_expr)
  in
  role_at types ~extent:binding.pvb_pat.ppat_loc annotation
;;

let role_of_type types annotation = role_at types (role_of_annotation annotation)
let method_at ?annotation ?extent types = role_at types ?extent (function_of annotation)
