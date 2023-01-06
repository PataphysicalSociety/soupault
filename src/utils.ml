include Soupault_common
open Defaults

(* IO helpers that return [(unit, string) result] instead of raising exceptions. *)

(* Reads a file and returns its content as a string. *)
let read_file file =
  try Ok (Soup.read_file file)
  with Sys_error msg ->
    Error (Printf.sprintf "Failed to read tile %s" msg)

(* Writes a string to a file. *)
let write_file file content =
  try
    let chan = open_out file in
    let () = Soup.write_channel chan content in
    let () = close_out chan in
    Ok ()
  with Sys_error msg -> Error msg

(* Creates a copy of a file with the same name in a different directory.
   If the target directory doesn't exist, creates that directory first.
 *)
let copy_file fs d =
  try
    let () = FileUtil.mkdir ~parent:true d in
    Ok (FileUtil.cp fs d)
  with
  | FileUtil.CpError msg -> Error msg
  | FileUtil.MkdirError msg -> Error msg


(* Result'y equivalents of higher-order list functions.
   Since soupault has an option to stop on page processing errors or continue despite them
   ([settings.strict] in the config or [--strict <true|false>>] in the command line),
   there needs to be a way to iterate over page lists of collect data from page processing
   that would allow ignoring errors.
   Raising an isn't an option since it would interrupt the caller,
   so the only way to achieve that is to make custom functions and use a sum type to signal errors.
   The built-in result type suits that purpose perfectly.
 *)

(* Result-aware iteration that can either stop on errors or ignore them. *)
let rec iter_result ?(ignore_errors=false) ?(fmt=(fun x -> x)) (f: 'a -> ('b, string) result) xs =
  match xs with
  | [] -> Ok ()
  | x :: xs ->
    let res = f x in
    begin
      match res with
      | Ok _ -> iter_result ~ignore_errors:ignore_errors ~fmt:fmt f xs
      | Error msg as e ->
        if ignore_errors then let () = Logs.warn @@ fun m -> m "%s" (fmt msg) in Ok ()
        else e
    end

(* Result-aware fold that can either stop on errors or ignore them. *)
let rec fold_left_result ?(ignore_errors=false) ?(fmt=(fun x -> x)) (f: 'a -> 'b -> ('c, string) result) acc xs =
  match xs with
  | [] -> Ok acc
  | x :: xs ->
    let acc' = f acc x in
    begin
      match acc' with
      | Ok acc' -> fold_left_result ~ignore_errors:ignore_errors ~fmt:fmt f acc' xs
      | Error msg as e ->
        if ignore_errors then
          let () = Logs.warn @@ fun m -> m "%s" (fmt msg) in
          fold_left_result ~ignore_errors:ignore_errors ~fmt:fmt f acc xs
        else e
    end


(* List helpers. *)

(* Exception-safe list tail function that assumes that empty list's tail is an empty list. *)
let safe_tl xs =
    match xs with
    | [] -> []
    | _ :: xs' -> xs'

(* Removes the last element of a list. *)
let drop_tail xs = List.rev xs |> safe_tl |> List.rev

(* Shortcut for checking if a list has a certain element, like [if x in xs] in Python.
   I still wonder why there's no such shortcut in either the standard library
   or in ocaml-containers when in lots of cases the [(=)] structural comparison
   function is really all you need.
 *)
let in_list needle haystack = List.exists ((=) needle) haystack

(* Checks if any elements from one list are present in another list. *)
let any_in_list needles haystack =
  List.fold_left (fun acc needle -> (in_list needle haystack) || acc) false needles

(* A map-like function for assoc lists that applies a function to all values,
    but tells that function both values and keys they are associated with. *)
let assoc_map_key_values f kvs = List.map (fun (k, v) -> (k, f k v)) kvs


(* Makes a slug for the id attribute. *)
let slugify ?(lowercase=true) ?(regex=None) ?(sub=None) s =
  let regex = Option.value ~default:"[^a-zA-Z0-9\\-]" regex in
  let sub = Option.value ~default:"-" sub in
  let s = Re.replace ~all:true ~f:(fun _ -> sub) (Re.Perl.compile_pat regex) s in
  if lowercase then String.lowercase_ascii s else s

(* Date/time helpers. *)

let rec parse_date fmts date_string =
  match fmts with
  | [] ->
    let () = Logs.debug @@ fun m -> m {|Field value "%s" could not be parsed as a date, interpreting as a string|} date_string in
    None
  | f :: fs -> begin
    let parser = ODate.Unix.From.generate_parser f in
    match parser with
    | None -> soupault_error (Printf.sprintf {|Date format "%s" is invalid|} f)
    | Some parser ->
      try
        let date = ODate.Unix.From.string parser date_string in
        let () = Logs.debug @@ fun m -> m {|Date string "%s" matched format "%s"|} date_string f in
        Some date
      with _ -> parse_date fs date_string
  end

let format_date fmt date =
  let printer = ODate.Unix.To.generate_printer fmt in
  match printer with
  | None -> soupault_error (Printf.sprintf {|Date format "%s" is invalid|} fmt)
  | Some printer -> ODate.Unix.To.string printer date


(* TOML/JSON convertors *)

let string_of_float f =
  if f = (Float.round f) then int_of_float f |> string_of_int
  else string_of_float f

(* Ezjsonm.value_to_string will always quote primitives, which is not always convenient.
   This is an alternate version that just returns bare string representations of JSON primitives.
 *)
let string_of_json_primitive j =
  match j with
  | `String s -> s
  | `Float f -> string_of_float f
  | `Bool b -> string_of_bool b
  | `Null -> "null"
  | _ -> failwith (Printf.sprintf "Expected a JSON primitive, got %s" (Ezjsonm.value_to_string j))

let rec toml_of_json j =
  let open Otoml in
  match j with
  | `Float n -> TomlFloat n
  | `Bool b -> TomlBoolean b
  | `String s -> TomlString s
  | `A js -> TomlArray (List.map toml_of_json js)
  | `O os -> TomlTable (List.map (fun (k, v) -> (k, toml_of_json v)) os)
  | `Null -> TomlTable []

let rec toml_to_json t =
  let open Otoml in
  match t with
  | TomlString s -> `String s
  | TomlInteger i -> `Float (float_of_int i)
  | TomlFloat f -> `Float f
  | TomlBoolean b -> `Bool b
  | TomlLocalTime s -> `String s
  | TomlLocalDate s -> `String s
  | TomlLocalDateTime s -> `String s
  | TomlOffsetDateTime s -> `String s
  | TomlArray xs | TomlTableArray xs -> `A (List.map toml_to_json xs)
  | TomlTable os | TomlInlineTable os -> `O (List.map (fun (k, v) -> (k, toml_to_json v)) os)

let json_of_index_entry e =
  let fields = [
    ("url", `String e.index_entry_url);
    ("page_file", `String e.index_entry_page_file);
    ("nav_path", `A (List.map (fun x -> `String x) e.index_entry_nav_path))
  ] in
  let fields = List.append fields e.fields in
  `O fields

let json_of_index_entries es =
  `A (List.map json_of_index_entry es)


(* Soupault-specific functions that are used in more than one place
   and thus needs to be in a shared module.
 *)

(* Checks if given build profile is present in the current list of build profiles
   (specified in the command line with [--profile] options).
 *)
let build_profile_matches profile build_profiles =
  (* Processing steps should run unless they have a "profile" option
     and it doesn't match the current build profile. *)
  match profile, build_profiles with
  | None, _ ->
    (* Widget/hook is not limited to any build profile,
       it should always run.
     *)
    true
  | Some _, [] ->
    (* Widget/hook is limited to a build profile but soupault is not run with any --profile options,
       so it cannot possibly match any given profile.
     *)
    false
  | Some p, _ ->
    (* Widget/hook is limited to a build profile and some profiles are given in the CLI options,
       so we need to actually check if that profile is in the list.
     *)
    Option.is_some @@ List.find_opt ((=) p) build_profiles

(* Loads plugin or hook code from given config options. *)
let load_plugin_code plugin_config default_filename ident =
  let file = Otoml.Helpers.find_string_opt plugin_config ["file"] in
  let source = Otoml.Helpers.find_string_opt plugin_config ["lua_source"] in
  match file, source with
  | None, None ->
    Error (Printf.sprintf {|In %s: either "file" or "lua_source" option is required|} ident)
  | Some _, Some _ ->
    Error (Printf.sprintf {|In %s: "file" and "lua_source" options are mutually exclusive|} ident)
  | None, Some source ->
    Ok (default_filename, source)
  | Some file, None ->
    try
      let source = Soup.read_file file in
      Ok (file, source)
     with Sys_error msg ->
       Error (Printf.sprintf "Could not read file %s: %s" file msg)

(* Warns about a deprecated option *)
let deprecation_warning f opt msg config =
  let value = f opt config in
  match value with
  | None -> ()
  | Some _ -> Logs.warn @@ fun m -> m "Deprecated option %s: %s" opt msg
