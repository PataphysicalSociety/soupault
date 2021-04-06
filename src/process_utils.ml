type process_status = Exited of int | Signaled of int | Stopped of int | CausedException of string

let of_unix_status s =
  match s with
  | Unix.WEXITED n -> Exited n
  | Unix.WSIGNALED n -> Signaled n
  | Unix.WSTOPPED n -> Stopped n

(* Logging process output is a debug aid if it succeeded,
   but it's an important diagnostic output if it failed,
   so we need adjustable log level here. *)
let log_process_outputs ?(func=Logs.debug) cmd out err =
  func @@ fun m ->
    m "Running \"%s\" produced the following outputs:\n Standard output:\n%s\nStandard error:\n%s" cmd out err

(* Produces a numeric exit code and a process execution result.
   Numeric codes mimic the bash convention, with one addition:
   254 = "process execution caused an exception" *)
let exit_code_of_status s =
  match s with
  | Exited num -> num
  | Signaled num -> 128 + num
  | Stopped num -> 128 + num
  | CausedException _ -> 254

(* Produces a human-readable error description. *)
let string_of_status s =
  match s with
  | Exited 0 -> "process exited normally"
  | Exited num -> Printf.sprintf "process exited with code %d" num
  | Signaled num -> Printf.sprintf "process was killed by signal %d" num
  | Stopped num -> Printf.sprintf "process was stopped by signal %d" num
  | CausedException msg -> Printf.sprintf "process execution caused an exception:\n%s" msg

let format_error cmd status =
  match status with
  | Exited 0 -> Printf.sprintf "Command \"%s\" has run successfully" cmd
  | _ -> Printf.sprintf "Failed to run command \"%s\": %s" cmd (string_of_status status)

(* Executes an external program and returns its stdout if it succeeds.
   Returns a raw, detailed exit status if it fails.
 *)
let get_program_output_raw ?(input=None) ?(debug=false) ?(env=[| |]) command =
  (* open_process_full does not automatically pass the existing environment
     to the child process, so we need to add it to our custom environment. *)
  let env = Array.append (Unix.environment ()) env in
  let (res, output, err_output) =
  begin try
    let std_out, std_in, std_err = Unix.open_process_full command env in
    let () =
      begin match input with
      | None -> ()
      | Some i ->
        let () = Logs.debug @@ fun m -> m "Data sent to program \"%s\": %s" command i in
        let () = Soup.write_channel std_in i; flush std_in in
        (* close stdin to signal the end of input *)
        close_out std_in
      end
    in
    let output = Soup.read_channel std_out in
    let err = Soup.read_channel std_err in
    let res = Unix.close_process_full (std_out, std_in, std_err) in
    let status = of_unix_status res in
    (status, output, err)
  with
  | Sys_error msg ->
    (* This is especially relevant on Windows.
       Since Windows doesn't have POSIX signals,
       conditions like "child process died" are signalled with Sys_error exceptions instead.

       XXX: can we preserve the partial stdout/stderr that we could read before the exception is occured?
     *)
    (CausedException (Printf.sprintf "System error: %s" msg), "", "")
  | _ ->
    let msg = Printexc.get_backtrace () in
    (CausedException msg, "", "")
  end
  in
  match res with
  | Exited 0 ->
    (* If a process completes successfully, we only want to log the outputs if debug output is enabled,
       else it's just a noise. *)
    let () = if debug then log_process_outputs command output err_output in
    Ok output
  | _ ->
    (* However, when a process fails, we want to always log the outputs even if debug is disabled
       because that information is usually essential for diagnosing the failure. *)
    let () = log_process_outputs ~func:Logs.err command output err_output in
    Error res

let get_program_output ?(input=None) ?(debug=false) ?(env=[| |]) command =
  let res = get_program_output_raw ~input:input ~debug:debug ~env:env command in
  match res with
  | Ok _ as o -> o
  | Error e -> Error (format_error command e)

