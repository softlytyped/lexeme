open! Core

(* A switch holds the standard library and an installed library side by side, and the
   project builds its own module elsewhere. *)
let root = Filename_unix.temp_dir "lexeme" ""

let make path file =
  let directory = Filename.concat root path in
  Core_unix.mkdir_p directory;
  Out_channel.write_all (Filename.concat directory file) ~data:""
;;

let%expect_test "the project builds a module whose interface lies outside the switch" =
  make "switch/lib/ocaml" "stdlib.cmi";
  make "switch/lib/library" "library.cmi";
  make "project/_build/source" "source.cmi";
  let build_path =
    List.map
      [ "project/_build/source"; "switch/lib/library"; "switch/lib/ocaml" ]
      ~f:(Filename.concat root)
  in
  let is_built_here name = Session.is_built_here ~build_path name in
  print_s
    [%sexp
      (List.map [ "Source"; "Library"; "Stdlib"; "Missing" ] ~f:is_built_here : bool list)];
  [%expect {| (true false false false) |}]
;;
