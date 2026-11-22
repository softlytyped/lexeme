open! Core

module Source = struct
  type t =
    { path : string
    ; text : string
    ; starts : int array
    }

  let source_of_string ~path text =
    let starts = String.substr_index_all text ~pattern:"\n" ~may_overlap:false in
    { path; text; starts = Array.of_list (0 :: List.map starts ~f:Int.succ) }
  ;;

  let path_of source = source.path
  let text_of source = source.text
end

type role =
  | Function
  | Variable
[@@deriving equal, sexp_of]

type result =
  | Boolean
  | Optional_boolean
  | Fallible_boolean
  | Other of string
  | Unknown
[@@deriving equal, sexp_of]

type binding =
  { name : string
  ; role : role
  ; result : result Lazy.t
  ; path : string
  ; line : int
  ; column : int
  ; letter_exception : string option
  }
[@@deriving sexp_of]

let rec offset_after_space text offset =
  match String.get text offset with
  | (' ' | '\t') when offset + 1 < String.length text ->
    offset_after_space text (offset + 1)
  | _ -> offset
;;

let exception_at text ~offset ~opening ~closing =
  let open Option.Let_syntax in
  let%bind () = Option.some_if (offset < String.length text) () in
  let start = offset_after_space text offset in
  let%bind () =
    Option.some_if (String.is_substring_at text ~pos:start ~substring:opening) ()
  in
  let body = start + String.length opening in
  let%bind stop = String.substr_index text ~pos:body ~pattern:closing in
  let comment = String.sub text ~pos:body ~len:(stop - body) in
  let%bind reason =
    String.chop_prefix (String.strip comment) ~prefix:"lexeme: allow-letter "
  in
  let reason = String.strip reason in
  Option.some_if ((not (String.is_empty reason)) && not (String.mem comment '\n')) reason
;;

let binding_at ?(result = lazy Unknown) (source : Source.t) ~offset ~name role =
  let open Option.Let_syntax in
  let%bind () =
    Option.some_if
      (offset >= 0
       && offset < String.length source.text
       && (not (String.is_empty name))
       && String.is_substring_at source.text ~pos:offset ~substring:name)
      ()
  in
  let row =
    match
      Array.binary_search
        source.starts
        ~compare:Int.compare
        `Last_less_than_or_equal_to
        offset
    with
    | Some row -> row
    | None -> 0
  in
  let opening, closing =
    match Filename.split_extension source.path |> snd with
    | Some ("ml" | "mli") -> "(*", "*)"
    | _ -> "/*", "*/"
  in
  Some
    { name
    ; role
    ; result
    ; path = source.path
    ; line = row + 1
    ; column = offset - source.starts.(row) + 1
    ; letter_exception =
        exception_at source.text ~offset:(offset + String.length name) ~opening ~closing
    }
;;

type 'result algebra =
  { bind : binding -> 'result
  ; enclose : 'result list -> 'result
  }
