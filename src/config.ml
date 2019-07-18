open Defaults

let default d o = CCOpt.get_or ~default:d o

(* List all keys of a TOML table
   This is used to retrieve a list of widgets to call
 *)
let list_config_keys table =
  TomlTypes.Table.fold (fun k _ ks -> (TomlTypes.Table.Key.to_string k) :: ks ) table []

(** Checks if config file exists.
    When config doesn't exist, soupault uses default settings,
    so this is considered a normal condition.

    When we cannot even check if it exists or not, however, something is clearly
    so wrong that there's no point in doing anything else.
    *)
let config_exists file =
  try
     FileUtil.test (FileUtil.Exists) Defaults.config_file
  with Unix.Unix_error (errno, _, _) ->
    let msg = Unix.error_message errno in
    let () = Logs.warn @@ fun m -> m "Could not check if config file %s exists: %s" file msg in
    exit 1

(** Read and parse a TOML file *)
let read_config path =
  if not (config_exists path) then
    let () = Logs.warn @@ fun m -> m "Configuration file %s not found, using default settings" path in
    Ok None
  else
  try
    let open Toml.Parser in
    let conf = from_filename path |> unsafe in
    Ok (Some conf)
  with
  | Sys_error err -> Error (Printf.sprintf "Could not read config fil: %s" err)
  | Toml.Parser.Error (err, _) -> Error (Printf.sprintf "Could not parse config file %s: %s" path err)

let get_table name config = TomlLenses.(get config (key name |-- table))

let get_string k tbl = TomlLenses.(get tbl (key k |-- string))
let get_string_default default_value k tbl = get_string k tbl |> default default_value
let get_string_result err k tbl = get_string k tbl |> CCOpt.to_result err

let get_bool k tbl = TomlLenses.(get tbl (key k |-- bool))
let get_bool_default default_value k tbl = get_bool k tbl |> default default_value
let get_bool_result err k tbl = get_bool k tbl |> CCOpt.to_result err

let get_int k tbl = TomlLenses.(get tbl (key k |-- int))
let get_int_default default_value k tbl = get_int k tbl |> default default_value
let get_int_result err k tbl = get_int k tbl |> CCOpt.to_result err

let get_strings k tbl = TomlLenses.(get tbl (key k |-- array |-- strings))
let get_strings_default default_value k tbl = get_strings k tbl |> default default_value
let get_strings_result err k tbl = get_strings k tbl |> CCOpt.to_result err

(** Converts a TOML table to an assoc list using a given value retrieval function,
    ignoring None's it may return.
  *)
let assoc_of_table f tbl =
  let has_value (_, v) = match v with Some _ -> true | None -> false in
  let keys = list_config_keys tbl in
  List.fold_left (fun xs k -> (k, f k tbl ) :: xs) [] keys |>
  List.filter has_value |>
  List.map (fun (k, v) -> k, Utils.unwrap_option v)

(** Tries to get a string list from a config
    If there's actually a string list, just return it.
    If there's a single string, consider it a single item list.
    If there's nothing like a string at all, return an empty list.
 *)
let get_strings_relaxed ?(default=[]) k tbl =
  let strs = get_strings k tbl in
  match strs with
  | Some strs -> strs
  | None -> begin
      let str = get_string k tbl in
      match str with
      | Some str -> [str]
      | None -> default
    end

let string_of_assoc xs =
  let xs = List.map (fun (k, v) -> Printf.sprintf "%s = %s" k v) xs in
  String.concat ", " xs

(* Update global settings with values from the config, if there are any *)
let _get_preprocessors config =
  let pt = get_table Defaults.preprocessors_table config in
  match pt with
  | None -> []
  | Some pt -> assoc_of_table get_string pt

let _get_index_settings settings config =
  let st = get_table Defaults.index_settings_table config in
  match st with
  | None -> settings
  | Some st ->
    {settings with
       index = get_bool_default settings.index "index" st;
       index_selector = get_string_default settings.index_selector "index_selector" st;
       index_title_selector = get_strings_relaxed ~default:settings.index_title_selector "index_title_selector" st;
       index_excerpt_selector = get_strings_relaxed ~default:settings.index_excerpt_selector "index_excerpt_selector" st;
       index_date_selector = get_strings_relaxed ~default:settings.index_date_selector "index_date_selector" st;
       index_author_selector = get_strings_relaxed ~default:settings.index_author_selector "index_author_selector" st;
       index_date_format = get_string_default settings.index_date_format "index_date_format" st;
       index_item_template = get_string_default settings.index_item_template "index_item_template" st;
       index_processor = get_string "index_processor" st;
    }

let _update_settings settings config =
  let st = get_table Defaults.settings_table config in
  match st with
  | None ->
     let () = Logs.warn @@ fun m -> m "Could not find the [settings] table in the config, using defaults" in
     settings
  | Some st ->
    {default_settings with
       verbose = get_bool_default settings.verbose "verbose" st;
       strict = get_bool_default settings.strict "strict" st;
       site_dir = get_string_default settings.site_dir "site_dir" st;
       build_dir = get_string_default settings.build_dir "build_dir" st;
       content_selector = get_string_default settings.content_selector "content_selector" st;
       doctype = get_string_default settings.doctype "doctype" st;
       index_page = get_string_default settings.index_page "index_page" st;
       index_file = get_string_default settings.index_file "index_file" st;
       default_template = get_string_default settings.default_template "default_template" st;
       clean_urls = get_bool_default settings.clean_urls "clean_urls" st;

       preprocessors = _get_preprocessors config
     }

let update_settings settings config =
  match config with
  | None -> settings
  | Some config ->
    let settings = _update_settings settings config in
    _get_index_settings settings config
