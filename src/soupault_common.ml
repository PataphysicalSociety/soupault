(* Common exceptions and helpers. *)

(* Indicates a page processing error.
   Such errors are ignored when [settings.strict] is set to false.
 *)
exception Soupault_error of string
let soupault_error s = raise (Soupault_error s)

(* Indicates an unrecoverable error in soupault's own logic:
   unimplemented edge case, broken invariant etc.
   It MUST NOT be ever handled so that the user can get a full exception trace
   and either debug the problem or report it to the maintainers.
 *)
exception Internal_error of string
let internal_error err =
  let () =
    Logs.err @@ fun m -> m "soupault encountered an internal error: %s." err;
    if not (Printexc.backtrace_status ()) then
      (* --debug or debug=true in the config enable exception trace recording early in the startup process.
         If exception trace recording isn't enabled, it likely means soupault is not running with debug on
         (or there's a logic error in initialization ;).
       *)
      Logs.err @@ fun m ->  m "You can run soupault --debug to get an exception trace. Please report it as a bug!"
    else
      Logs.err @@ fun m ->  m "Please report a bug and attach the message and the exception trace to your report."
  in raise (Internal_error err)

(* Indicates that soupault encountered a file name that is impossible in the operating system it's running on.

   When it occurs inside soupault's own code (as opposed to plugin code),
   it should always be re-raised as Internal_error.
 *)
exception Malformed_file_name of string

(* Custom infix operators *)

let (+/) left right =
    FilePath.concat left right
