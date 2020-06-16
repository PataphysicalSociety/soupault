
exception Soupault_error of string

let soupault_error s = raise (Soupault_error s)

(** Reads a file and return its content *)
let get_file_content file =
  try Ok (Soup.read_file file)
  with Sys_error msg -> Error msg

let parse_html ?(body=true) str =
  let context = if body then `Fragment "body" else `Fragment "head" in
  Markup.string str |> Markup.parse_html ~context:context |> Markup.signals |> Soup.from_signals

(* Result wrapper for FileUtil.cp *)
let cp fs d =
  try Ok (FileUtil.cp fs d)
  with FileUtil.CpError msg -> Error msg

let wrap_select f selector soup =
  try Ok (f selector soup)
  with Soup.Parse_error msg -> Error (Printf.sprintf "Invalid CSS selector '%s', parse error: %s" selector msg)

let select selector soup = wrap_select Soup.select selector soup
let select_one selector soup = wrap_select Soup.select_one selector soup

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

(** Unsafely unwraps an option type.
    There are many places where None is easy to prove to not happen *)
let unwrap_option o =
  match o with
  | Some v -> v
  | None -> raise (Failure "values of beta will give rise to dom!")

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
  let (let*) = Stdlib.Result.bind in
  let* e = select_one selector soup in
  match e with
  | Some e -> Ok e
  | None -> Error err

(** Adds class to an element if class is given *)
let add_class c e =
  match c with
  | Some c -> Soup.add_class c e
  | None -> ()

(** Extracts all text nodes from an element and its children if there're any,
    and returns them all as a single string.
    Essentially, strips HTML tags from element's inner HTML. *)
let get_element_text e =
  let texts = Soup.texts e in
  match texts with
  | [] -> None
  | _ ->
    (* "Normalize" the whitespace *)
    let text = String.concat "" texts |> String.trim in
    if text = "" then None else Some text

let rec select_any_of selectors soup =
  match selectors with
  | [] -> None
  | s :: ss ->
    let e =
      try Soup.select_one s soup
      with Soup.Parse_error msg -> Printf.ksprintf soupault_error "Invalid CSS selector '%s', parse error: %s" s msg
    in
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
        let nodes =
          try Soup.select s soup
          with Soup.Parse_error msg -> Printf.ksprintf soupault_error "Invalid CSS selector '%s', parse error: %s" s msg
        in
        let acc = List.append (Soup.to_list nodes) acc in
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
let inner_html ?(escape_html=true) e =
  let children = child_nodes e in
  if escape_html then Soup.to_string children
  else
      let escape_text = fun (x:string) -> x in
      let escape_attribute = fun (x:string) -> x in
      children |> Soup.signals |> (fun s -> Markup.write_html ~escape_text ~escape_attribute s) |> Markup.to_string

(** Appends a child if child rather than None is given *)
let append_child container child =
  match child with
  | None -> ()
  | Some c -> Soup.append_child container c

(** Replaces all content of a container node with something else *)
let replace_content container content =
  let () = Soup.iter Soup.delete (Soup.children container) in
  Soup.append_child container content

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

(** Inserts a node into the page at desired position *)
let insert_element action container content =
  let actions =
    [("append_child", Soup.append_child); ("prepend_child", Soup.prepend_child);
     ("insert_before", Soup.insert_before); ("insert_after", Soup.insert_after);
     ("replace_element", Soup.replace); ("replace_content", replace_content);
     ("ignore_output", fun _ _ -> ())]
  in
  let action_fun = CCList.assoc_opt ~eq:(=) action actions in
  match action_fun with
  | Some f -> f container content
  | None ->
    let index = Spellcheck.make_index (assoc_keys actions) in
    let suggestion = Spellcheck.get_suggestion index action in
    let suggestion = (match suggestion with Some s -> (Printf.sprintf " Did you mean \"%s?\"" s) | None -> "") in
    let () = Logs.warn @@ fun m -> m "Invalid action \"%s\", using default (append child).%s" action suggestion in
    Soup.append_child container content

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

(** Fixup for FilePath.get_extension raising Not_found for files without extensions *)
let get_extension file =
  try FilePath.get_extension file
  with Not_found -> ""

(* Remove trailing slashes from a path.
   This is mainly to work around fileutils#14 issue.
 *)
let normalize_path path =
  (* If a path is empty, leave it empty.
     Right now soupault treates build_dir="" as
     "use current working dir for build dir",
     until we all decide whether it's a good idea or not,
     let's keep it an easily removable line.
   *)
  if path = "" then path else
  let path = Re.replace  ~f:(fun _ -> "") (Re.Perl.compile_pat "/$") path in
  if path = "" then "/" else path
