open! Core

(* We give the child a seekable input so it may write any amount of output before it
   reads. Its standard error stays connected to the caller's standard error. *)
let with_input input reader =
  match input with
  | None -> In_channel.with_file "/dev/null" ~f:reader
  | Some contents ->
    let path, channel = Stdlib.Filename.open_temp_file "lexeme" ".input" in
    Exn.protect
      ~finally:(fun () ->
        Stdlib.close_out_noerr channel;
        Stdlib.Sys.remove path)
      ~f:(fun () ->
        Out_channel.output_string channel contents;
        Out_channel.close channel;
        In_channel.with_file path ~f:reader)
;;

let read ?input ?(environment = []) program arguments =
  let open Or_error.Let_syntax in
  let%bind output, status =
    Or_error.try_with (fun () ->
      with_input input (fun channel ->
        let process =
          Core_unix.create_process_with_fds
            ~env:(`Override environment)
            ~prog:program
            ~args:arguments
            ~stdin:(Use_this (Core_unix.descr_of_in_channel channel))
            ~stdout:Generate
            ~stderr:(Use_this Core_unix.stderr)
            ()
        in
        let stream = Core_unix.in_channel_of_descr process.stdout in
        let output =
          Or_error.try_with (fun () ->
            Exn.protect
              ~f:(fun () -> In_channel.input_all stream)
              ~finally:(fun () -> In_channel.close stream))
        in
        output, Core_unix.waitpid process.pid))
  in
  match status with
  | Ok () -> output
  | Error status ->
    Or_error.error_s
      [%message
        "program exited unsuccessfully"
          program
          (arguments : string list)
          (status : Core_unix.Exit_or_signal.error)]
;;
