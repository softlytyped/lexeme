open! Core

module Outcome = struct
  type t =
    | Accepted
    | Rejected
    | Failed

  (* The exit statuses that the manual documents. *)
  let status_of = function
    | Accepted -> 0
    | Rejected -> 1
    | Failed -> 2
  ;;
end

let paths_in directory =
  let patterns = [ "*.ml"; "*.mli"; "*.c"; "*.h" ] in
  let arguments = [ "-C"; directory; "ls-files"; "-z"; "--cached"; "--others" ] in
  let%map.Or_error output =
    Process.run "git" (arguments @ [ "--exclude-standard"; "--" ] @ patterns)
  in
  let locate path =
    match directory with
    | "." -> path
    | _ -> directory ^/ path
  in
  String.split output ~on:'\000'
  |> List.filter_map ~f:(fun path ->
    Option.some_if (not (String.is_empty path)) (locate path))
  |> List.filter ~f:Sys_unix.file_exists_exn
;;

let files_of paths =
  let paths_at path =
    match Sys_unix.is_directory path with
    | `Yes -> paths_in path
    | `No | `Unknown -> Ok [ path ]
  in
  let paths =
    match paths with
    | [] -> [ "." ]
    | _ :: _ -> paths
  in
  let%map.Or_error groups = Or_error.combine_errors (List.map paths ~f:paths_at) in
  List.concat groups |> List.dedup_and_sort ~compare:String.compare
;;

let program_of (configuration : Configuration.t) path =
  let { Configuration.directories; definitions; _ } = configuration in
  let%bind.Or_error text = Or_error.try_with (fun () -> In_channel.read_all path) in
  match snd (Filename.split_extension path) with
  | Some ("ml" | "mli") -> Ocaml_syntax.read ~path text
  | Some ("c" | "h") -> C_syntax.read ~path ~directories ~definitions text
  | _ -> Or_error.error_s [%message "expected an OCaml or C source file" path]
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
  match List.exists diagnostics ~f:is_error with
  | true -> Outcome.Rejected
  | false -> Accepted
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
    Failed
;;

let settings_of ~configuration_path ~directories ~definitions =
  let open Or_error.Let_syntax in
  let%bind configuration = Configuration.load ?path:configuration_path () in
  let%map lexicon = Lexicon.extend (force Lexicon.english) configuration.vocabulary in
  let configuration =
    { configuration with
      directories = configuration.directories @ directories
    ; definitions = configuration.definitions @ definitions
    }
  in
  let exemptions = String.Set.of_list configuration.exemptions in
  ( configuration
  , Judgement.algebra_of lexicon ~limits:configuration.function_lines ~exemptions )
;;

let run ~configuration_path ~directories ~definitions paths =
  let checks =
    let%bind.Or_error configuration, algebra =
      settings_of ~configuration_path ~directories ~definitions
    in
    let%map.Or_error files = files_of paths in
    List.map files ~f:(check configuration algebra)
  in
  match checks with
  | Ok results -> report results
  | Error error -> report [ Error error ]
;;

let command =
  Command.basic
    ~summary:"Check the names and the function lengths of OCaml and C sources"
    (let%map_open.Command configuration_path =
       flag
         "-config"
         (optional string)
         ~doc:"FILE settings (default: the nearest .lexeme)"
     and directories = flag "-I" (listed string) ~doc:"DIR add a C include directory"
     and definitions =
       flag "-D" (listed string) ~doc:"MACRO define a C preprocessor macro"
     and paths = anon (sequence ("FILE-OR-DIRECTORY" %: string)) in
     fun () ->
       exit (Outcome.status_of (run ~configuration_path ~directories ~definitions paths)))
;;

let () = Command_unix.run command
