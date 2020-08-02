include Soupault_common

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
      Printf.fprintf std_in "%s" i
  in
  (* close stdin to flag end of input *)
  let () = close_out std_in in
  let output = Soup.read_channel std_out in
  let err = Soup.read_channel std_err in
  let res = Unix.close_process_full (std_out, std_in, std_err) in
  match res with
  | Unix.WEXITED 0 -> Ok output
  | _ -> Error (Printf.sprintf "Failed to execute \"%s\": %s%s" command output err)

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

(** Result-aware iteration *)
let rec iter ?(ignore_errors=false) ?(fmt=(fun x -> x)) f xs =
  match xs with
  | [] -> Ok ()
  | x :: xs ->
    let res = f x in
    begin
      match res with
      | Ok _ -> iter f xs
      | Error msg as e  ->
        if ignore_errors then let () = Logs.warn @@ fun m -> m "%s" (fmt msg) in Ok ()
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
  print_endline "Copyright 2020 Daniil Baturin";
  print_endline "Soupault is free software distributed under the MIT license";
  print_endline "Visit https://soupault.neocities.org/reference-manual for documentation"

(** Warns about a deprecated option *)
let deprecation_warning f opt msg config =
  let value = f opt config in
  match value with
  | None -> ()
  | Some _ -> Logs.warn @@ fun m -> m "Deprecated option %s: %s" opt msg

(* Replaces all URL-unsafe characters with hyphens *)
let slugify s =
  Re.Str.global_replace (Re.Str.regexp "[^a-zA-Z0-9\\-]") "-" s |>
  String.lowercase_ascii

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

(* Remove trailing slashes from a path.
   This is mainly to work around fileutils#14 issue.
 *)
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
