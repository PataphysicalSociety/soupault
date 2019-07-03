open Defaults

let default d o = CCOpt.get_or ~default:d o

(* Option monad *)
let (>>=) = CCOpt.(>>=)

(* List all keys of a TOML table
   This is used to retrieve a list of widgets to call
 *)
let list_config_keys table =
  TomlTypes.Table.fold (fun k _ ks -> (TomlTypes.Table.Key.to_string k) :: ks ) table []

(* Read and parse a TOML file *)
let read_config path =   
  try
    let open Toml.Parser in
    let conf = from_filename path |> unsafe in
    Ok conf
  with
  | Sys_error err -> Error (Printf.sprintf "Could not read config file %s" err)
  | Toml.Parser.Error (err, _) -> Error (Printf.sprintf "Could not parse config file %s: %s" path err)

let get_table name config = TomlLenses.(get config (key name |-- table))

let get_string k tbl = TomlLenses.(get tbl (key k |-- string))
let get_string_default default_value k tbl = get_string k tbl |> default default_value

(* Update global settings with values from the config, if there are any *)
let update_settings settings config =
  let st = TomlLenses.(get config (key "settings" |-- table)) in
  match st with
  | None ->
     let () = Logs.warn @@ fun m -> m "Could not find the [settings] table in the config, using defaults" in
     settings
  | Some st ->
    let verbose = TomlLenses.(get st (key "verbose" |-- bool)) |> default settings.verbose in
    let strict = TomlLenses.(get st (key "strict" |-- bool)) |> default settings.strict in
    let site_dir = TomlLenses.(get st (key "site_dir" |-- string)) |> default settings.site_dir in
    let build_dir = TomlLenses.(get st (key "build_dir" |-- string)) |> default settings.build_dir in
    let content_selector = TomlLenses.(get st (key "content_selector" |-- string)) |> default settings.content_selector in
    {settings with
       verbose = verbose;
       strict = strict;
       site_dir = site_dir;
       build_dir = build_dir;
       content_selector = content_selector
     }
