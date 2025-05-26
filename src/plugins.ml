open Common

module OH = Otoml.Helpers

let (>>=) = Option.bind

(* Plugin config loading *)
let get_plugin_config config plugin =
  let plugins_tbl = Config.find_table_opt ["plugins"; plugin] config in
  match plugins_tbl with
  | Some plugins_tbl -> plugins_tbl
  | None ->
    (* This function is, or should be used only with plugin names already
       retrieved from the config *)
    Printf.ksprintf internal_error "Trying to lookup a non-existent plugin %s" plugin

let list_plugins config =
  let ps = Config.find_table_opt ["plugins"] config >>= (fun x -> Some (Otoml.list_table_keys x)) in
  match ps with
  | None -> []
  | Some ps' -> ps'

let get_plugins soupault_config =
  let load_plugin soupault_config hash name =
    let plugin_cfg = get_plugin_config soupault_config name in
    let () = Config.check_options ["file"; "lua_source"] plugin_cfg "a plugin config" in
    let default_filename = Printf.sprintf "<inline Lua source for plugin %s>" name in
    let ident = Printf.sprintf "plugin %s" name in
    let (file_name, source) = Utils.load_plugin_code plugin_cfg default_filename ident in
    Hashtbl.add hash name (Plugin_api.run_plugin file_name source)
  in
  let () = Logs.info @@ fun m -> m "Loading explicitly configured plugins" in
  let plugins_hash = Hashtbl.create 1024 in
  let plugin_names = list_plugins soupault_config in
  let () = List.iter (load_plugin soupault_config plugins_hash) plugin_names in
  plugins_hash

let rec lookup_plugin_file plugin_dirs file_name =
  match plugin_dirs with
  | [] -> None
  | d :: ds ->
    let file_path = FilePath.concat d file_name in
    if FileUtil.test FileUtil.Exists file_path then Some file_path
    else lookup_plugin_file ds file_name
