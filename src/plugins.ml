module OH = Otoml.Helpers

let (>>=) = Option.bind

(* Plugin config loading *)
let get_plugin_config config plugin =
  let plugins_tbl = Config.find_table_opt [Defaults.plugins_table; plugin] config in
  match plugins_tbl with
  | Some plugins_tbl -> plugins_tbl
  | None ->
    (* This function is, or should be used only with plugin names already
       retrieved from the config *)
   failwith @@ Printf.sprintf "Trying to lookup a non-existent plugin %s" plugin

let list_plugins config =
  let ps = Config.find_table_opt [Defaults.plugins_table] config >>= (fun x -> Some (Otoml.list_table_keys x)) in
  match ps with
  | None -> []
  | Some ps' -> ps'

let make_plugin_function lua_source settings config name =
  let plugin_env_ref = Plugin_api.make_plugin_env () in
  Plugin_api.run_plugin settings config name lua_source plugin_env_ref

let rec _load_plugins settings ps config hash =
  let (let*) = Stdlib.Result.bind in
  match ps with
  | [] -> Ok ()
  | p :: ps' ->
    let plugin_cfg = get_plugin_config config p in
    let () = Config.check_options ["file"; "lua_source"] plugin_cfg "a plugin config" in
    let default_filename = Printf.sprintf "<inline Lua source for plugin %s>" p in
    let ident = Printf.sprintf "plugin %s" p in
    let* (file_name, source) = Utils.load_plugin_code plugin_cfg default_filename ident in
    let () =  Hashtbl.add hash p (make_plugin_function source settings config file_name) in
    _load_plugins settings ps' config hash

let get_plugins settings config =
  let (let*) = Stdlib.Result.bind in
  let hash = Hashtbl.create 1024 in
  match config with
  | None -> Ok hash
  | Some config ->
    let plugins = list_plugins config in
    let* () = _load_plugins settings plugins config hash in
    Ok hash

let rec lookup_plugin_file plugin_dirs file_name =
  match plugin_dirs with
  | [] -> None
  | d :: ds ->
    let file_path = FilePath.concat d file_name in
    if FileUtil.test FileUtil.Exists file_path then Some file_path
    else lookup_plugin_file ds file_name
