(** Reads a file and return its content *)
let get_file_content file =
  try Ok (Soup.read_file file)
  with Sys_error msg -> Error msg

(** Executes an external program and return its stdout *)
let get_program_output command env_array =
  let std_out, std_in, std_err = Unix.open_process_full command env_array  in
  let output = Soup.read_channel std_out in
  let err = Soup.read_channel std_err in
  let res = Unix.close_process_full (std_out, std_in, std_err) in
  match res with
  | Unix.WEXITED 0 -> Ok output
  | _ -> Error (Printf.sprintf "Failed to execute \"%s\": %s" command err)

(** Exception-safe list tail function that assumes that empty list's
    tail is an empty list. Used for breadcrumbs. *)
let safe_tl xs =
    match xs with
    | [] -> []
    | _ :: xs' -> xs'

(** Unsafely unwraps an option type.
    There are many places where None is easy to prove to not happen *)
let unwrap_option o =
  match o with
  | Some v -> v
  | None -> raise (Failure "values of beta will give rise to dom!")

(** Checks if a "template" has a specific element in it.
    For checking if there's any element at all, use "*" selector *)
let check_template selector template =
  let soup = Soup.parse template in
  let content_container = Soup.select_one selector soup in
  match content_container with
  | None -> Error (Printf.sprintf "Template %s has no element matching selector \"%s\"" template selector)
  | Some _ -> Ok template

let add_class c e =
  match c with
  | Some c -> Soup.add_class c e
  | None -> ()

(** Just prints a hardcoded program version *)
let print_version () =
  print_endline "soupault 0.9";
  print_endline "Copyright 2019 Daniil Baturin, licensed under MIT";
  print_endline "Visit https://baturin.org/projects/soupault for documentation"
