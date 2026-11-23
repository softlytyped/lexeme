open! Core

(* We give the child a file to read, rather than a pipe, so that it may write any amount
   of output before it reads its input. *)
let with_input input ~read =
  match input with
  | None -> In_channel.with_file "/dev/null" ~f:read
  | Some contents ->
    let path = Filename_unix.temp_file "lexeme" ".input" in
    Exn.protect
      ~finally:(fun () -> Sys_unix.remove path)
      ~f:(fun () ->
        Out_channel.write_all path ~data:contents;
        In_channel.with_file path ~f:read)
;;

let contents_of descriptor =
  let stream = Core_unix.in_channel_of_descr descriptor in
  Exn.protect
    ~f:(fun () -> In_channel.input_all stream)
    ~finally:(fun () -> In_channel.close stream)
;;

let child_of program arguments channel =
  let input = Core_unix.Fd_spec.Use_this (Core_unix.descr_of_in_channel channel) in
  let errors = Core_unix.Fd_spec.Use_this Core_unix.stderr in
  Core_unix.create_process_with_fds
    ~prog:program
    ~args:arguments
    ~stdin:input
    ~stdout:Generate
    ~stderr:errors
    ()
;;

let output_of program arguments channel =
  let child = child_of program arguments channel in
  let output = contents_of child.stdout in
  output, Core_unix.waitpid child.pid
;;

let failure_of program arguments status =
  Or_error.error_s
    [%message
      "expected the program to exit successfully"
        program
        (arguments : string list)
        (status : Core_unix.Exit_or_signal.error)]
;;

let run ?input program arguments =
  match
    Or_error.try_with (fun () -> with_input input ~read:(output_of program arguments))
  with
  | Error _ as failure -> failure
  | Ok (output, Ok ()) -> Ok output
  | Ok (_, Error status) -> failure_of program arguments status
;;
