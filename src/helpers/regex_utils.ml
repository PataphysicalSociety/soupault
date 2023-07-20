open Soupault_common

(* Internal exception to signal malformed (incorrect or unsupported) regex input.
   Unfortunately, as of 1.10.3, ocaml-rere doesn't give meaningful parse error messages,
   so there's nothing to tell the user,
   which is why this exception constuction is nullary.
 *)
exception Bad_regex

let format_error regex = Printf.sprintf "failed to parse regex %s" regex

(* ocaml-re supports multiple regex types, but soupault only exposes Perl-compatible ones
   because they are most common and familiar to users.
 *)
let compile_regex regex =
  try Re.Perl.compile_pat regex
  with Re__Perl.Parse_error | Re__Perl.Not_supported ->
    raise Bad_regex

(* These are base functions.
   They raise Bad_regex errors that shouldn't be visible to the user.
   The user should get either Internal_error if a _hardcoded_ regex is malformed,
   or Soupault_error if a _user-supplied_ regex is malformed.
   Thus code that uses these functions must handle Bad_regex
   and convert it to its own Soupault_error with a descriptive message
   (e.g. tell the user which config option bad regex comes from.
 *)
module Raw = struct
  let get_matching_strings ~regex str =
    let re = compile_regex regex in
    Re.matches re str

  let matches ~regex str =
    let res = get_matching_strings ~regex:regex str in
    match res with
    | [] -> false
    | _  -> true

  let replace ?(all=false) ~regex ~sub str =
    let re = compile_regex regex in
    Re.replace ~all:all ~f:(fun _ -> sub) re str

  let split ~regex str =
    let re = compile_regex regex in
    Re.split re str
end

(* Internal functions meant to be used with hardcoded regexes.
   Failures of these functions expose logic errors and should be treated as fatal.
   That's why they raise the never-handled Internal_error exception.
 *)
module Internal = struct
  let get_matching_strings ~regex str =
    try Raw.get_matching_strings ~regex:regex str
    with Bad_regex -> internal_error @@ format_error regex

  let matches ~regex string =
    try Raw.matches ~regex:regex string
    with Bad_regex -> internal_error @@ format_error regex

  let replace ?(all=false) ~regex ~sub str =
    try Raw.replace ~all:all ~regex:regex ~sub:sub str
    with Bad_regex -> internal_error @@ format_error regex

  let split ~regex string =
    try Raw.split ~regex:regex string
    with Bad_regex -> internal_error @@ format_error regex
end

(* Functions that accept regexes from user input.
   Their failures are usually user failures,
   and the user can choose whether to ignore page processing error or not.
 *)
module Public = struct
  let get_matching_strings ~regex str =
    try Raw.get_matching_strings ~regex:regex str
    with Bad_regex -> soupault_error @@ format_error regex

  let matches ~regex string =
    try Raw.matches ~regex:regex string
    with Bad_regex -> soupault_error @@ format_error regex

  let replace ?(all=false) ~regex ~sub str =
    try Raw.replace ~all:all ~regex:regex ~sub str
    with Bad_regex -> soupault_error @@ format_error regex

  let split ~regex string =
    try Raw.split ~regex:regex string
    with Bad_regex -> soupault_error @@ format_error regex
end
