open Soupault_common

(* File name and path manipulation helpers. *)

(* Splits a file name into its "proper name" and extensions. *)
let split_file_name file =
  let parts = String.split_on_char '.' file in
  match parts with
  | [] | "" :: [] | "" :: "" :: _ ->
    (* UNIX-like OSes and Windows alike disallow empty file names
       and file names that consist entirely of dots,
       so if this function gets a malformed path,
       the user should be notified that something went wrong.
     *)
    raise @@ Malformed_file_name file
  | "" :: name :: extensions ->
    (* If a name starts with a dot, we consider its part before the second dot its "base" name.
       As in, [strip_extensions(".bashrc.gz")] will return ".bashrc".
       That's the most sensible behavior I can think of.
     *)
    (("." ^ name), extensions)
  | name :: extensions ->
    (name, extensions)

(* Gets all extensions from a file name. *)
let get_extensions file =
  let (_, extensions) = split_file_name file in
  extensions

(* Gets the last extension of a file name, or returns an empty string if there are no extensions. *)
let get_extension file =
  let (_, extensions) = split_file_name file in
  match (List.rev extensions) with
  | [] -> ""
  | ext :: _ -> ext

(* Removes all extensions from a file name. *)
let strip_extensions file =
  let (name, _) = split_file_name file in
  name

(* Checks if a file name has a certain extension in it.
   It doesn't have to be the last extension:
   [has_extension "tar" "file.tar.gz"] is true.
 *)
let has_extension extension file =
  let extensions = get_extensions file in
  let res = List.find_opt ((=) extension) extensions in
  match res with
  | None -> false
  | Some _ -> true

(* Removes trailing slashes from a path. *)
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

(* Joins a list of file path parts into a full path
   according to the convention of the OS that soupault is running on.
 *)
let concat_path fs = List.fold_left FilePath.concat "" fs

(* Splits a file path into its components
   according to the convention of the OS that soupault is running on.
 *)
let split_path p =
  let sep = if Sys.win32 then {|(\\)+|} else "(/)+" in
  Re.split (Re.Perl.compile_pat sep) p
