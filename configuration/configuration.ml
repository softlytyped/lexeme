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

let rec nearest_in directory =
  let candidate = directory ^/ file_name in
  let parent = Filename.dirname directory in
  match Sys_unix.file_exists candidate with
  | `Yes -> Some candidate
  | (`No | `Unknown) when String.equal parent directory -> None
  | `No | `Unknown -> nearest_in parent
;;

let resolve_directories configuration ~path =
  let resolve directory =
    match Filename.is_relative directory with
    | true -> Filename.dirname path ^/ directory
    | false -> directory
  in
  { configuration with directories = List.map configuration.directories ~f:resolve }
;;

let read path =
  Or_error.try_with (fun () -> t_of_sexp (Sexp.load_sexp path))
  |> Or_error.tag ~tag:("expected a Lexeme configuration in " ^ path)
  |> Or_error.map ~f:(resolve_directories ~path)
;;

let load ?path () =
  match Option.first_some path (nearest_in (Sys_unix.getcwd ())) with
  | Some path -> read path
  | None -> Ok (t_of_sexp (Sexp.List []))
;;
