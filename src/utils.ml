(** Reads a file and return its content *)
let get_file_content file =
  try Ok (Soup.read_file file)
  with Sys_error msg -> Error msg

(* Result wrapper for FileUtil.cp *)
let cp fs d =
  try Ok (FileUtil.cp fs d)
  with FileUtil.CpError msg -> Error msg

(** Executes an external program and returns its stdout *)
let get_program_output ?(input=None) command env_array =
  (* On Windows, child process environment is completely overwritten
     if you use open_process_full, so we add %PATH% by hand here to allow
     calling external programs *)
  let env_array =
    if Sys.os_type = "Win32" then Array.append env_array [|("PATH=" ^ Unix.getenv "PATH")|]
    else env_array
  in
  let std_out, std_in, std_err = Unix.open_process_full command env_array in
  let () =
    match input with
    | None -> ()
    | Some i ->
      let () = Logs.debug @@ fun m -> m "JSON index data: %s" i in
      Printf.fprintf std_in "%s\n%!" i
  in
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

(** Removes the last element of a list *)
let drop_tail xs = List.rev xs |> safe_tl |> List.rev

(** Shortcut for checking if a list has this element *)
let in_list xs x = List.exists ((=) x) xs

(** Unsafely unwraps an option type.
    There are many places where None is easy to prove to not happen *)
let unwrap_option o =
  match o with
  | Some v -> v
  | None -> raise (Failure "values of beta will give rise to dom!")

(** Result-aware iteration *)
let rec iter f xs =
  match xs with
  | [] -> Ok ()
  | x :: xs ->
    let res = f x in
    begin
      match res with
      | Ok _ -> iter f xs
      | Error _ as e -> e
    end

(** Checks if a "template" has a specific element in it.
    For checking if there's any element at all, use "*" selector *)
let check_template selector template =
  let soup = Soup.parse template in
  let content_container = Soup.select_one selector soup in
  match content_container with
  | None -> Error (Printf.sprintf "Template %s has no element matching selector \"%s\"" template selector)
  | Some _ -> Ok template

(** Gets an element and returns Error if it doesn't exist,
    another bit of monadic convenience *)
let get_required_element selector err soup =
  let e = Soup.select_one selector soup in
  match e with
  | Some e -> Ok e
  | None -> Error err

(** Adds class to an element if class is given *)
let add_class c e =
  match c with
  | Some c -> Soup.add_class c e
  | None -> ()

(** Gets the first text node from an element if there's any
    non-empty text in it *)
let get_element_text e =
  let texts = Soup.texts e in
  match texts with
  | [] -> None
  | _ ->
    (* "Normalize" the whitespace *)
    let texts = List.map String.trim texts in
    let text = String.concat " " texts |> String.trim in
    if text = "" then None else Some text

let rec select_any_of selectors soup =
  match selectors with
  | [] -> None
  | s :: ss ->
    let e = Soup.select_one s soup in
    begin
      match e with
      | Some _ as e -> e
      | None -> select_any_of ss soup
    end

let select_all selectors soup =
  let rec aux selectors soup acc =
    match selectors with
    | [] -> acc
    | s :: ss ->
        let acc = List.append (Soup.select s soup |> Soup.to_list) acc in
        aux ss soup acc
  in aux selectors soup []

(** Creates a soup with child nodes of an element ripped out of their context *)
let child_nodes e =
  (* XXX: Rendering and re-parsing seems to be the only way to
     rip nodes out of their parent context now,
     and thus make them safe for adding to a different place *)
  let cs = Soup.children e |> Soup.to_list |> List.map (fun e -> Soup.to_string e |> Soup.parse) in
  let soup = Soup.create_soup () in
  let () = List.iter (Soup.append_root soup) cs in
  soup

(** Retrieves the innerHTML of an element --
    a string representation of its children *)
let inner_html e =
  let children = child_nodes e in
  Soup.to_string children

(** Appends a child if child rather than None is given *)
let append_child container child =
  match child with
  | None -> ()
  | Some c -> Soup.append_child container c

(** Checks if a node is empty

    A node is considered empty iff it has no children but whitespace nodes.
    If a node has no children, it's clearly empty, and if it has more than one,
    then clearly isn't.
    The interesting case is one child. Sadly, lambdasoup has no function for checking
    node type now, so instead we check if its content is empty when converted
    to a string and stripped of whitespace.
 *)
let is_empty node =
  let open Soup in
  let children = children node in
  match (Soup.count children) with
  | 0 -> true
  | 1 -> (children |> first |> unwrap_option |> to_string |> String.trim) = ""
  | _ -> false

(** Just a convenience function for Re.matches *)
let get_matching_strings r s =
  try
    let re = Re.Perl.compile_pat r in
    Ok (Re.matches re s)
  with Re__Perl.Parse_error -> Error (Printf.sprintf "Failed to parse regex %s" r)

(** Just prints a hardcoded program version *)
let print_version () =
  Printf.printf "soupault %s\n" Defaults.version;
  print_endline "Copyright 2019 Daniil Baturin, licensed under MIT";
  print_endline "Visit https://baturin.org/projects/soupault for documentation"

(** Warns about a deprecated option *)
let deprecation_warning f opt msg config =
  let value = f opt config in
  match value with
  | None -> ()
  | Some _ -> Logs.warn @@ fun m -> m "Deprecated option %s: %s" opt msg
