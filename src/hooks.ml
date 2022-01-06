module I = Plugin_api.I

let lua_of_toml = Plugin_api.lua_of_toml

let hook_types = ["save"]

let hook_should_run settings hook_config hook_type page_file =
  let disabled = Config.find_bool_or ~default:false hook_config ["disabled"] in
  if disabled then
    let () = Logs.debug @@ fun m -> m "%s hook is disabled in the configuration" hook_type in false
  else
  let options = Config.get_path_options hook_config in
  if Path_options.page_included settings options settings.site_dir page_file then true
  else
    let () = Logs.debug @@ fun m -> m "%s hook is not used: page %s is excluded by its page/section/regex options"
      hook_type page_file
    in false

let run_save_hook settings soupault_config hook_config file_name lua_code env page_source =
  let open Defaults in
  let lua_str = I.Value.string in
  let state = I.mk () in
   let () =
    (* Set up the save hook environment *)
    I.register_globals ["page_source", lua_str.embed page_source] state;
    I.register_globals ["page_file", lua_str.embed env.page_file] state;
    I.register_globals ["target_file", lua_str.embed env.target_file] state;
    I.register_globals ["target_dir", lua_str.embed env.target_dir] state;
    I.register_globals ["config", lua_of_toml hook_config] state;
    I.register_globals ["hook_config", lua_of_toml hook_config] state;
    I.register_globals ["soupault_config", lua_of_toml soupault_config] state;
    I.register_globals ["force", I.Value.bool.embed settings.force] state;
    I.register_globals ["build_dir", lua_str.embed settings.build_dir] state;
    I.register_globals ["site_dir", lua_str.embed settings.site_dir] state;
  in
  Plugin_api.run_lua file_name state lua_code

let check_hook_tables config =
  let hooks_table = Config.find_table_opt ["hooks"] config in
  match hooks_table with
  | None -> ()
  | Some tbl -> Config.check_subsections ~parent_path:["hooks"] tbl hook_types "hooks"

let load_hook hook_config ident =
  let default_filename = Printf.sprintf "<inline Lua source for hook \"%s\">" ident in
  let ident = Printf.sprintf "hook \"%s\"" ident in
  let res = Utils.load_plugin_code hook_config default_filename ident in
  match res with
  | Ok (file_name, source_code) -> (file_name, source_code)
  | Error msg -> Config.config_error msg

let get_hook config hooks_hash ident =
  let save_hook_config = Config.find_table_opt ["hooks"; ident] config in
  match save_hook_config with
  | None -> ()
  | Some shc ->
    let (file_name, source) = load_hook shc ident in
    Hashtbl.add hooks_hash ident (file_name, source, shc)

let _load_hooks config =
  let () = check_hook_tables config in
  let hooks_hash = Hashtbl.create 1024 in
  let () = List.iter (get_hook config hooks_hash) hook_types in
  hooks_hash

let get_hooks config =
  try Ok (_load_hooks config)
  with Config.Config_error msg -> Error msg
