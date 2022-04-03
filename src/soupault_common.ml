exception Soupault_error of string
let soupault_error s = raise (Soupault_error s)

(* This exception indicates an unrecoverable error in soupault's own logic:
   unimplemented edge case, broken invariant etc.
   It MUST NOT be ever handled so that the user can get a full exception trace
   and either debug the problem or report it to the maintainers.
 *)
exception Internal_error of string
let internal_error s =
  let msg = Printf.sprintf "soupault encountered an internal error: %s.\
    You can run soupault --debug to get an exception trace.\
    Please report it as a bug!" s
  in raise (Internal_error msg)
