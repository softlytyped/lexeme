open! Core

module Outcome = struct
  type t =
    | Acceptance
    | Rejection
    | Failure

  (* The exit statuses that the manual documents. *)
  let status_of = function
    | Acceptance -> 0
    | Rejection -> 1
    | Failure -> 2
  ;;
end

type flags =
  { configuration_path : string option
  ; directories : string list
  ; definitions : string list
  }

(* Git lists the tracked files and the untracked files that it does not ignore, relative
   to the directory. A tracked file may since have been deleted. *)
let listing_of directory =
  let arguments = [ "-C"; directory; "ls-files"; "-z"; "--cached"; "--others" ] in
  Process.run
    "git"
    (arguments @ [ "--exclude-standard"; "--"; "*.ml"; "*.mli"; "*.c"; "*.h" ])
;;

let location_of directory path =
  if String.equal directory "." then path else directory ^/ path
;;

let is_deleted path =
  match Sys_unix.file_exists path with
  | `No -> true
  | `Yes | `Unknown -> false
;;

let paths_in directory =
  let%map.Or_error listing = listing_of directory in
  String.split listing ~on:'\000'
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:(location_of directory)
  |> List.filter ~f:(Fn.non is_deleted)
;;

let paths_at path =
  match Sys_unix.is_directory path with
  | `Yes -> paths_in path
  | `No | `Unknown -> Ok [ path ]
;;

let files_of paths =
  let paths = if List.is_empty paths then [ "." ] else paths in
  let%map.Or_error groups = Or_error.combine_errors (List.map paths ~f:paths_at) in
  List.dedup_and_sort (List.concat groups) ~compare:String.compare
;;

let reader_of (configuration : Configuration.t) path =
  let { Configuration.directories; definitions; _ } = configuration in
  match snd (Filename.split_extension path) with
  | Some ("ml" | "mli") -> Ok (Ocaml_syntax.read ~path)
  | Some ("c" | "h") -> Ok (C_syntax.read ~path ~directories ~definitions)
  | _ -> Or_error.error_s [%message "expected an OCaml or C source file" path]
;;

let program_of configuration path =
  let open Or_error.Let_syntax in
  let%bind read = reader_of configuration path in
  let%bind text = Or_error.try_with (fun () -> In_channel.read_all path) in
  read text
;;

let check configuration algebra path =
  program_of configuration path
  |> Or_error.map ~f:(fun program ->
    Judgement.diagnostics_of (Syntax.fold algebra program))
  |> Or_error.tag ~tag:path
;;

let outcome_of diagnostics =
  let is_error diagnostic =
    Diagnostic.Severity.equal (Diagnostic.severity_of diagnostic) Error
  in
  if List.exists diagnostics ~f:is_error then Outcome.Rejection else Acceptance
;;

let report results =
  let diagnostics, failures = List.partition_result results in
  let diagnostics = List.concat diagnostics in
  List.iter diagnostics ~f:(fun diagnostic ->
    print_endline (Diagnostic.to_string diagnostic));
  match failures with
  | [] -> outcome_of diagnostics
  | _ :: _ ->
    eprintf "%s\n%!" (Error.to_string_hum (Error.of_list failures));
    Failure
;;

let with_flags (configuration : Configuration.t) flags =
  { configuration with
    directories = configuration.directories @ flags.directories
  ; definitions = configuration.definitions @ flags.definitions
  }
;;

let settings_of flags =
  let open Or_error.Let_syntax in
  let%bind configuration = Configuration.load ?path:flags.configuration_path () in
  let%map lexicon = Lexicon.extend (force Lexicon.english) configuration.vocabulary in
  let limits = configuration.function_lines in
  let exemptions = String.Set.of_list configuration.exemptions in
  with_flags configuration flags, Judgement.algebra_of lexicon ~limits ~exemptions
;;

let checks_of flags paths =
  let open Or_error.Let_syntax in
  let%bind configuration, algebra = settings_of flags in
  let%map files = files_of paths in
  List.map files ~f:(check configuration algebra)
;;

let run flags paths =
  match checks_of flags paths with
  | Ok results -> report results
  | Error error -> report [ Error error ]
;;

let command =
  Command.basic
    ~summary:"Check the names, types, branches and lengths of OCaml and C definitions"
    (let%map_open.Command configuration_path =
       flag
         "-config"
         (optional string)
         ~doc:"FILE settings (default: the nearest .lexeme)"
     and directories = flag "-I" (listed string) ~doc:"DIR add a C include directory"
     and definitions =
       flag "-D" (listed string) ~doc:"MACRO define a C preprocessor macro"
     and paths = anon (sequence ("FILE-OR-DIRECTORY" %: string)) in
     let flags = { configuration_path; directories; definitions } in
     fun () -> exit (Outcome.status_of (run flags paths)))
;;

let () = Command_unix.run command
