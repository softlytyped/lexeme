open! Core
open Parsetree
open Binding_syntax

let base = Ast_iterator.default_iterator

(* The compiler visits the expression and the annotation inside a constraint. We use that
   traversal for the several constraint representations used by OCaml and OxCaml. *)
let rec is_function expression =
  match expression.pexp_desc with
  | Pexp_function _ -> true
  | Pexp_letop { body; _ } | Pexp_open (_, body) | Pexp_sequence (_, body) ->
    is_function body
  | Pexp_constraint _ | Pexp_coerce _ | Pexp_newtype _ | Pexp_let _ ->
    let result = ref false in
    let inspect evidence = result := !result || evidence in
    base.expr
      { base with
        expr = (fun _ body -> inspect (is_function body))
      ; typ = (fun _ annotation -> inspect (Typing.is_function annotation))
      ; value_binding = (fun _ _ -> ())
      }
      expression;
    !result
  | _ -> false
;;

let role_of_type description =
  match Typing.is_function description with
  | true -> Function
  | false -> Variable
;;

let annotation_of traversal =
  let annotation = ref None in
  traversal
    { base with
      typ = (fun _ description -> annotation := Some description)
    ; pat = (fun _ _ -> ())
    ; expr = (fun _ _ -> ())
    ; case = (fun _ _ -> ())
    ; attribute = (fun _ _ -> ())
    };
  !annotation
;;

let shape_of binding =
  let shape_of_type annotation = role_of_type annotation, Some annotation in
  match binding.pvb_constraint with
  | Some (Pvc_constraint { typ = annotation; _ }) -> shape_of_type annotation
  | Some (Pvc_coercion { coercion; _ }) -> shape_of_type coercion
  | None ->
    let pattern = binding.pvb_pat in
    let annotation =
      match pattern.ppat_desc with
      | Ppat_constraint _ -> annotation_of (fun visitor -> base.pat visitor pattern)
      | _ -> None
    in
    (match annotation, binding.pvb_expr.pexp_desc with
     | Some annotation, _ -> shape_of_type annotation
     | None, Pexp_function _ ->
       Function, annotation_of (fun visitor -> base.expr visitor binding.pvb_expr)
     | _ ->
       ( (match is_function binding.pvb_expr with
          | true -> Function
          | false -> Variable)
       , None ))
;;

let is_identifier name =
  match String.is_empty name with
  | true -> false
  | false ->
    String.for_all name ~f:(fun letter ->
      Char.is_alphanum letter
      || Char.equal letter '_'
      || Char.equal letter '\''
      || Char.to_int letter > Char.to_int '\127')
;;

let fold source algebra =
  let typing = Typing.types_of source in
  let bind
    ?extent
    ?annotation
    ?(inference = false)
    writer
    role
    (name : string Location.loc)
    =
    match is_identifier name.txt with
    | false -> ()
    | true ->
      let role, annotation =
        match annotation, role, inference with
        | None, Variable, true ->
          let annotation = Typing.annotation_at typing name in
          Option.value_map annotation ~default:role ~f:role_of_type, annotation
        | _ -> role, annotation
      in
      let result = lazy (Typing.result_at ?extent typing name ~annotation) in
      binding_at ~result source ~offset:name.loc.loc_start.pos_cnum ~name:name.txt role
      |> Option.iter ~f:(fun binding -> writer (algebra.bind binding))
  in
  let children_of traversal =
    let children = Queue.create () in
    traversal (Queue.enqueue children);
    Queue.to_list children
  in
  let rec visitor_of writer : Ast_iterator.iterator =
    let enter (traversal : Ast_iterator.iterator -> unit) =
      match children_of (fun collector -> traversal (visitor_of collector)) with
      | [] -> ()
      | children -> writer (algebra.enclose children)
    in
    let rec pattern ?annotation ?(inference = false) visitor role node =
      match node.ppat_desc with
      | Ppat_var name -> bind ?annotation ~inference writer role name
      | Ppat_alias (body, name) ->
        pattern ?annotation ~inference visitor role body;
        bind ?annotation ~inference writer role name
      | Ppat_constraint _ ->
        base.pat
          { visitor with
            pat = (fun visitor body -> pattern ?annotation ~inference visitor role body)
          }
          node
      | _ -> base.pat visitor node
    in
    { base with
      attribute = (fun _ _ -> ())
    ; pat = (fun visitor node -> pattern visitor Variable node)
    ; value_binding =
        (fun visitor binding ->
          let role, annotation = shape_of binding in
          pattern ?annotation ~inference:true visitor role binding.pvb_pat;
          enter (fun visitor -> visitor.expr visitor binding.pvb_expr))
    ; value_description =
        (fun _ declaration ->
          bind
            ~annotation:declaration.pval_type
            writer
            (role_of_type declaration.pval_type)
            declaration.pval_name)
    ; label_declaration =
        (fun _ label ->
          bind
            ~annotation:label.pld_type
            writer
            (role_of_type label.pld_type)
            label.pld_name)
    ; type_declaration =
        (fun _ declaration ->
          enter (fun visitor -> base.type_declaration visitor declaration))
    ; class_structure =
        (fun _ body -> enter (fun visitor -> base.class_structure visitor body))
    ; class_signature =
        (fun _ body -> enter (fun visitor -> base.class_signature visitor body))
    ; class_field =
        (fun visitor field ->
          (match field.pcf_desc with
           | Pcf_val (name, _, _) | Pcf_inherit (_, _, Some name) ->
             bind writer Variable name
           | Pcf_method (name, _, Cfk_virtual annotation) ->
             bind ~annotation writer Function name
           | Pcf_method (name, _, _) -> bind ~extent:field.pcf_loc writer Function name
           | _ -> ());
          base.class_field visitor field)
    ; class_type_field =
        (fun visitor field ->
          (match field.pctf_desc with
           | Pctf_val (name, _, _, _) -> bind writer Variable name
           | Pctf_method (name, _, _, annotation) -> bind ~annotation writer Function name
           | _ -> ());
          base.class_type_field visitor field)
    ; case = (fun _ branch -> enter (fun visitor -> base.case visitor branch))
    ; structure = (fun _ items -> enter (fun visitor -> base.structure visitor items))
    ; signature = (fun _ items -> enter (fun visitor -> base.signature visitor items))
    ; payload =
        (fun visitor payload ->
          match payload with
          | PStr items -> base.structure visitor items
          | _ -> base.payload visitor payload)
    ; expr =
        (fun visitor expression ->
          match expression.pexp_desc with
          | Pexp_let _ | Pexp_letop _ | Pexp_extension _ -> base.expr visitor expression
          | _ ->
            let descend _ body = enter (fun visitor -> visitor.expr visitor body) in
            base.expr { visitor with expr = descend } expression)
    }
  in
  let path = Source.path_of source in
  let parse () =
    let buffer = Lexing.from_string (Source.text_of source) in
    Location.init buffer path;
    algebra.enclose
      (children_of (fun writer ->
         let visitor = visitor_of writer in
         match Filename.split_extension path |> snd with
         | Some "mli" -> base.signature visitor (Parse.interface buffer)
         | _ -> base.structure visitor (Parse.implementation buffer)))
  in
  match parse () with
  | result -> Or_error.map (Typing.verify typing) ~f:(fun () -> result)
  | exception error ->
    (match Location.error_of_exn error with
     | Some _ ->
       Or_error.error_string (Format.asprintf "%a" Location.report_exception error)
     | None -> Or_error.of_exn error)
;;
