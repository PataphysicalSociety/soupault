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
  if not (config_exists path) then Ok None else
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

(* Update global settings with values from the config, if there are any *)
let _update_settings settings config =
  let st = get_table Defaults.settings_table config in
  match st with
  | None ->
     let () = Logs.warn @@ fun m -> m "Could not find the [settings] table in the config, using defaults" in
     settings
  | Some st ->
    {
       verbose = get_bool_default settings.verbose "verbose" st;
       strict = get_bool_default settings.strict "strict" st;
       site_dir = get_string_default settings.site_dir "site_dir" st;
       build_dir = get_string_default settings.build_dir "build_dir" st;
       content_selector = get_string_default settings.content_selector "content_selector" st;
       doctype = get_string_default settings.doctype "doctype" st;
       index_page = get_string_default settings.index_page "index_page" st;
       index_file = get_string_default settings.index_file "index_file" st;
       default_template = get_string_default settings.default_template "default_template" st
     }

let update_settings settings config =
  match config with
  | None -> settings
  | Some config -> _update_settings settings config

