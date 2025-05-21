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

let rec _load_plugins soupault_config ps hash =
  let (let*) = Stdlib.Result.bind in
  match ps with
  | [] -> Ok ()
  | p :: ps' ->
    let plugin_cfg = get_plugin_config soupault_config p in
    let () = Config.check_options ["file"; "lua_source"] plugin_cfg "a plugin config" in
    let default_filename = Printf.sprintf "<inline Lua source for plugin %s>" p in
    let ident = Printf.sprintf "plugin %s" p in
    let* (file_name, source) = Utils.load_plugin_code plugin_cfg default_filename ident in
    let () =  Hashtbl.add hash p (Plugin_api.run_plugin file_name source) in
    _load_plugins soupault_config ps' hash

let get_plugins soupault_config =
  let () = Logs.info @@ fun m -> m "Loading explicitly configured plugins" in
  let hash = Hashtbl.create 1024 in
  let plugins = list_plugins soupault_config in
  let res = _load_plugins soupault_config plugins hash in
  match res with
  | Ok () -> hash
  | Error msg ->
    Printf.ksprintf soupault_error "Failed to load plugins: %s" msg

let rec lookup_plugin_file plugin_dirs file_name =
  match plugin_dirs with
  | [] -> None
  | d :: ds ->
    let file_path = FilePath.concat d file_name in
    if FileUtil.test FileUtil.Exists file_path then Some file_path
    else lookup_plugin_file ds file_name
