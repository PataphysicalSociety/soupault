let (>>=) = Option.bind

(* Plugin config loading *)
let get_plugin_config config plugin =
  let plugins_tbl = Config.get_table Defaults.plugins_table config >>= Config.get_table plugin in
  match plugins_tbl with
  | Some plugins_tbl -> plugins_tbl
  | None ->
    (* This function is, or should be used only with plugin names already
       retrieved from the config *)
   failwith @@ Printf.sprintf "Trying to lookup a non-existent plugin %s" plugin

let list_plugins config =
  let ps = Config.get_table Defaults.plugins_table config >>= (fun x -> Some (Toml_utils.list_table_keys x)) in
  match ps with
  | None -> []
  | Some ps' -> ps'

let rec _load_plugins ps config hash =
  match ps with
  | [] -> Ok ()
  | p :: ps' ->
    let plugin_cfg = get_plugin_config config p in
    let () = Config.check_options ["file"] plugin_cfg "a plugin config" in
    let file = Config.get_string "file" plugin_cfg in
    begin
      match file with
      | None ->
        Error (Printf.sprintf "In plugin %s: missing required option \"file\"" p)
      | Some file ->
        try
          let lua_source = Soup.read_file file in
          Hashtbl.add hash p (Plugin_api.run_plugin file lua_source);
          _load_plugins ps' config hash
        with Sys_error msg ->
          Error (Printf.sprintf "Could not read plugin file %s: %s" file msg)
    end

let get_plugins config =
  let (let*) = Stdlib.Result.bind in
  let hash = Hashtbl.create 1024 in
  match config with
  | None -> Ok hash
  | Some config ->
    let plugins = list_plugins config in
    let* () = _load_plugins plugins config hash in
    Ok hash

let rec lookup_plugin_file plugin_dirs file_name =
  match plugin_dirs with
  | [] -> None
  | d :: ds ->
    let file_path = FilePath.concat d file_name in
    if FileUtil.test FileUtil.Exists file_path then Some file_path
    else lookup_plugin_file ds file_name
