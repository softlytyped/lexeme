open! Core

type configuration =
  { vocabulary : Lexicon.vocabulary [@default Lexicon.vocabulary_of_sexp (Sexp.List [])]
  ; directories : string list [@sexp.list]
  ; definitions : string list [@sexp.list]
  }
[@@deriving of_sexp]

let configuration_of file =
  let open Or_error.Let_syntax in
  let path = Option.value file ~default:".lexeme" in
  let%bind contents =
    match file, Sys_unix.file_exists ".lexeme" with
    | None, `No -> Ok "()"
    | _ -> Or_error.try_with (fun () -> In_channel.read_all path)
  in
  let%map configuration =
    Or_error.try_with (fun () -> configuration_of_sexp (Sexp.of_string contents))
    |> Or_error.tag ~tag:("expected a Lexeme configuration in " ^ path)
  in
  let directories =
    List.map configuration.directories ~f:(fun directory ->
      match Filename.is_relative directory with
      | true -> Filename.dirname path ^/ directory
      | false -> directory)
  in
  { configuration with directories }
;;

let paths_of directory =
  let open Or_error.Let_syntax in
  let%map output =
    Process.read
      "git"
      [ "-C"
      ; directory
      ; "ls-files"
      ; "-z"
      ; "--cached"
      ; "--others"
      ; "--exclude-standard"
      ; "--"
      ; "*.ml"
      ; "*.mli"
      ; "*.c"
      ; "*.h"
      ]
  in
  String.split output ~on:'\000'
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:(fun path ->
    match directory with
    | "." -> path
    | _ -> directory ^/ path)
;;

let files_of paths =
  let open Or_error.Let_syntax in
  let paths =
    match paths with
    | [] -> [ "." ]
    | _ -> paths
  in
  let%map groups =
    List.map paths ~f:(fun path ->
      match Sys_unix.is_directory path with
      | `Yes -> paths_of path
      | _ -> Ok [ path ])
    |> Or_error.combine_errors
  in
  List.concat groups |> List.dedup_and_sort ~compare:String.compare
;;

let check configuration lexicon path =
  let open Or_error.Let_syntax in
  let%bind text = Or_error.try_with (fun () -> In_channel.read_all path) in
  let source = Binding_syntax.Source.source_of_string ~path text in
  let algebra = Naming.algebra_of lexicon in
  let%map file =
    match Filename.split_extension path |> snd with
    | Some ("ml" | "mli") -> Ocaml_syntax.fold source algebra
    | Some ("c" | "h") ->
      C_syntax.fold
        source
        ~directories:configuration.directories
        ~definitions:configuration.definitions
        algebra
    | _ -> Or_error.error_s [%message "expected an OCaml or C source file" path]
  in
  Naming.diagnostics_of file
;;

let run file directories definitions paths =
  let open Or_error.Let_syntax in
  let%bind configuration = configuration_of file in
  let configuration =
    { configuration with
      directories = configuration.directories @ directories
    ; definitions = configuration.definitions @ definitions
    }
  in
  let%bind lexicon = Lexicon.extend (force Lexicon.english) configuration.vocabulary in
  let%bind paths = files_of paths in
  let results =
    List.map paths ~f:(fun path ->
      check configuration lexicon path |> Or_error.tag ~tag:path)
  in
  let diagnostics, failures = List.partition_result results in
  let diagnostics = List.concat diagnostics in
  List.iter diagnostics ~f:(fun diagnostic -> print_endline (Naming.format diagnostic));
  match failures with
  | _ :: _ -> Error (Error.of_list failures)
  | [] ->
    Ok
      (match List.exists diagnostics ~f:Naming.is_error with
       | true -> 1
       | false -> 0)
;;

let command =
  Command.basic
    ~summary:"Check the grammar of names in OCaml and C source"
    (let%map_open.Command file =
       flag
         "-config"
         (optional string)
         ~doc:"FILE read vocabulary and C options (default .lexeme)"
     and directories = flag "-I" (listed string) ~doc:"DIR add a C include directory"
     and definitions =
       flag "-D" (listed string) ~doc:"MACRO define a C preprocessor macro"
     and paths = anon (sequence ("FILE-OR-DIRECTORY" %: string)) in
     fun () ->
       match run file directories definitions paths with
       | Ok status -> Stdlib.exit status
       | Error error ->
         eprintf "%s\n%!" (Error.to_string_hum error);
         Stdlib.exit 2)
;;

let () = Command_unix.run command
