open! Core

type t =
  { vocabulary : Lexicon.Vocabulary.t [@default Lexicon.Vocabulary.empty]
  ; function_lines : Judgement.Limits.t [@default Judgement.Limits.default]
  ; exemptions : string list [@default [ "main"; "equal" ]]
  ; directories : string list [@sexp.list]
  ; definitions : string list [@sexp.list]
  }
[@@deriving of_sexp]

let file_name = ".lexeme"

let rec nearest_file_from directory =
  let candidate = directory ^/ file_name in
  let parent = Filename.dirname directory in
  match Sys_unix.file_exists candidate with
  | `Yes -> Some candidate
  | (`No | `Unknown) when String.equal parent directory -> None
  | `No | `Unknown -> nearest_file_from parent
;;

let resolve configuration ~path =
  let location_of directory =
    if Filename.is_relative directory
    then Filename.dirname path ^/ directory
    else directory
  in
  { configuration with directories = List.map configuration.directories ~f:location_of }
;;

let read path =
  Or_error.try_with (fun () -> t_of_sexp (Sexp.load_sexp path))
  |> Or_error.tag ~tag:("expected a Lexeme configuration in " ^ path)
  |> Or_error.map ~f:(resolve ~path)
;;

let load ?path () =
  match Option.first_some path (nearest_file_from (Sys_unix.getcwd ())) with
  | Some path -> read path
  | None -> Ok (t_of_sexp (Sexp.List []))
;;
