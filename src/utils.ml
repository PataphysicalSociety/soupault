include Soupault_common

type process_result = Output of string | ExecutionError of (Unix.process_status * string * string)

(** Reads a file and return its content *)
let get_file_content file =
  try Ok (Soup.read_file file)
  with Sys_error msg -> Error msg

(* Result wrapper for FileUtil.cp *)
let cp fs d =
  try
    let () = FileUtil.mkdir ~parent:true d in
    Ok (FileUtil.cp fs d)
  with
  | FileUtil.CpError msg -> Error msg
  | FileUtil.MkdirError msg -> Error msg

(** Executes an external program and returns its stdout *)
let get_program_output ?(input=None) command env_array =
  (* open_process_full does not automatically pass the existing environment
     to the child process, so we need to add it to our custom environment. *)
  let env_array = Array.append (Unix.environment ()) env_array in
  let std_out, std_in, std_err = Unix.open_process_full command env_array in
  let () =
    match input with
    | None -> ()
    | Some i ->
      let () = Logs.debug @@ fun m -> m "Data sent to program \"%s\": %s" command i in
      Soup.write_channel std_in i;
      flush std_in
  in
  (* close stdin to signal the end of input *)
  let () = close_out std_in in
  let output = Soup.read_channel std_out in
  let err = Soup.read_channel std_err in
  let res = Unix.close_process_full (std_out, std_in, std_err) in
  match res with
  | Unix.WEXITED 0 -> Output output
  | _ -> ExecutionError (res, output, err)

let format_process_error code =
  match code with
  | Unix.WEXITED 0 -> "process exited normally"
  | Unix.WEXITED num -> Printf.sprintf "process exited with code %d" num
  | Unix.WSIGNALED num -> Printf.sprintf "process was killed by signal %d" num
  | Unix.WSTOPPED num -> Printf.sprintf "process was stopped by signal %d" num

let log_process_error cmd out err =
  Logs.debug @@ fun m ->
    m "Running \"%s\" produced the following outputs:\n Standard output:\n%s\nStandard error:\n%s" cmd out err

let handle_process_error cmd res =
  match res with
  | Output out -> Ok out
  | ExecutionError (code, out, err) ->
    let () = log_process_error cmd out err in
    Error (Printf.sprintf "Failed to run \"%s\": %s" cmd (format_process_error code))

(** Exception-safe list tail function that assumes that empty list's
    tail is an empty list. Used for breadcrumbs. *)
let safe_tl xs =
    match xs with
    | [] -> []
    | _ :: xs' -> xs'

(** Removes the last element of a list *)
let drop_tail xs = List.rev xs |> safe_tl |> List.rev

(** Shortcut for checking if a list has this element *)
let in_list xs x = List.exists ((=) x) xs

(** Extracts keys from an assoc list *)
let assoc_keys xs = List.fold_left (fun acc (x, _) -> x :: acc) [] xs

let assoc_values xs = List.map (fun (_, v) -> v) xs

(** Result-aware iteration *)
let rec iter ?(ignore_errors=false) ?(fmt=(fun x -> x)) f xs =
  match xs with
  | [] -> Ok ()
  | x :: xs ->
    let res = f x in
    begin
      match res with
      | Ok _ -> iter ~ignore_errors:ignore_errors ~fmt:fmt f xs
      | Error msg as e ->
        if ignore_errors then let () = Logs.warn @@ fun m -> m "%s" (fmt msg) in Ok ()
        else e
    end

(* Result-aware fold *)
let rec fold_left ?(ignore_errors=false) ?(fmt=(fun x -> x)) f acc xs =
  match xs with
  | [] -> Ok acc
  | x :: xs ->
    let acc' = f acc x in
    begin
      match acc' with
      | Ok acc' -> fold_left ~ignore_errors:ignore_errors ~fmt:fmt f acc' xs
      | Error msg as e ->
        if ignore_errors then
          let () = Logs.warn @@ fun m -> m "%s" (fmt msg) in
          fold_left ~ignore_errors:ignore_errors ~fmt:fmt f acc xs
        else e
    end

(** Just a convenience function for Re.matches *)
let get_matching_strings r s =
  try
    let re = Re.Perl.compile_pat r in
    Ok (Re.matches re s)
  with Re__Perl.Parse_error -> Error (Printf.sprintf "Failed to parse regex %s" r)

(** Just prints a hardcoded program version *)
let print_version () =
  Printf.printf "soupault %s\n" Defaults.version_string;
  print_endline "Copyright 2021 Daniil Baturin";
  print_endline "Soupault is free software distributed under the MIT license";
  print_endline "Visit https://www.soupault.app/reference-manual for documentation"

(** Warns about a deprecated option *)
let deprecation_warning f opt msg config =
  let value = f opt config in
  match value with
  | None -> ()
  | Some _ -> Logs.warn @@ fun m -> m "Deprecated option %s: %s" opt msg

(* Makes a heading slug for the id attribute.
   In the "hard" mode it replaces everything but ASCII letters and digits with hyphens.
   In the "soft" mode it only replaces whitespace with hyphens.
   HTML standards only demand that id should not have whitespace in it,
   contrary to the popular opinion.
 *)
let slugify ?(lowercase=true) ?(regex=None) ?(sub=None) s =
  let regex = Option.value ~default:"[^a-zA-Z0-9\\-]" regex in
  let sub = Option.value ~default:"-" sub in
  let s = Re.replace ~all:true ~f:(fun _ -> sub) (Re.Perl.compile_pat regex) s in
  if lowercase then String.lowercase_ascii s else s

let profile_matches profile build_profile =
  (* Processing steps should run unless they have a "profile" option
     and it doesn't match the current build profile. *)
  match profile, build_profile with
  | None, _ -> true
  | Some _, None -> false
  | Some p, Some bp -> p = bp

(* Fixup for FilePath.get_extension raising Not_found for files without extensions.

   See https://github.com/gildor478/ocaml-fileutils/issues/12 for details.
 *)
let get_extension file =
  try FilePath.get_extension file
  with Not_found -> ""

(* Remove trailing slashes from a path. *)
let normalize_path path =
  (* If a path is empty, leave it empty.
     Right now soupault treats build_dir="" as
     "use the current working dir for build dir".
     Until we all decide whether it's a good idea or not,
     let's keep it an easily removable line.
   *)
  if path = "" then path else
  let path = Re.replace  ~f:(fun _ -> "") (Re.Perl.compile_pat "/$") path in
  if path = "" then "/" else path

let concat_path fs = List.fold_left FilePath.concat "" fs

let string_of_float f =
  if f = (Float.round f) then int_of_float f |> string_of_int
  else string_of_float f

(* Ezjsonm erroneously believes that JSON only allows arrays or objects at the top level.
   That hasn't been true for quite a while: bare numbers, strings etc. are valid JSON objects.
   This is a kludge for compensating for it.
 *)
let string_of_json_primitive j =
  match j with
  | `String s -> s
  | `Float f -> string_of_float f
  | `Bool b -> string_of_bool b
  | `Null -> "null"
  | _ -> failwith "Ezjsonm needs a fix for standards compliance"

let rec parse_date fmts date_string =
  match fmts with
  | [] ->
    let () = Logs.debug @@ fun m -> m "Field value \"%s\" could not be parsed as a date, interpreting as a string" date_string in
    None
  | f :: fs -> begin
    let parser = ODate.Unix.From.generate_parser f in
    match parser with
    | None -> soupault_error (Printf.sprintf "Date format \"%s\" is invalid." f)
    | Some parser ->
      try
        let date = ODate.Unix.From.string parser date_string in
        let () = Logs.debug @@ fun m -> m "Date string \"%s\" matched format \"%s\"" date_string f in
        Some date
      with Failure _ -> parse_date fs date_string
  end

let format_date fmt date =
  let printer = ODate.Unix.To.generate_printer fmt in
  match printer with
  | None -> soupault_error (Printf.sprintf "Date format \"%s\" is invalid." fmt)
  | Some printer -> ODate.Unix.To.string printer date
