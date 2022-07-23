open Defaults
open Soupault_common

module I = Plugin_api.I

let (let*) = Result.bind

let lua_of_toml = Plugin_api.lua_of_toml

let hook_types = ["pre-parse"; "pre-process"; "post-index"; "render"; "save"; "post-save"]

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

let run_render_hook settings soupault_config hook_config file_name lua_code env soup =
  let open Defaults in
  let lua_str = I.Value.string in
  let state = I.mk () in
  (* Get the index entry for the current page from the index hash *)
  let index_entry_json =
    begin
      let index_entry = Hashtbl.find_opt env.site_index_hash env.page_file in
      match index_entry with
      | None -> `Null
      | Some ie -> Utils.json_of_index_entry ie
    end
  in
  let () =
    (* Set up the hook environment *)
    I.register_globals [
      "page", Plugin_api.lua_of_soup (Plugin_api.Html.SoupNode soup);
      "page_file", lua_str.embed env.page_file;
      "page_url", lua_str.embed env.page_url;
      "site_index", Plugin_api.lua_of_json (Utils.json_of_index_entries env.site_index);
      "index_entry", Plugin_api.lua_of_json index_entry_json;
      "target_file", lua_str.embed env.target_file;
      "target_dir", lua_str.embed env.target_dir;
      "config", lua_of_toml hook_config;
      "hook_config", lua_of_toml hook_config;
      "soupault_config", lua_of_toml soupault_config;
      "force", I.Value.bool.embed settings.force;
      "build_dir", lua_str.embed settings.build_dir;
      "site_dir", lua_str.embed settings.site_dir
    ] state;
  in
  let (let*) = Result.bind in
  let () = Logs.info @@ fun m -> m "Running the render hook on page %s" env.page_file in
  let* () = Plugin_api.run_lua file_name state lua_code in
  let res = I.getglobal state (I.Value.string.embed "page_source") in
  if I.Value.string.is res then Ok (I.Value.string.project res)
  else Error "render hook has not assigned a string to the page_source variable"

let run_save_hook settings soupault_config hook_config file_name lua_code env page_source =
  let open Defaults in
  let lua_str = I.Value.string in
  let state = I.mk () in
   let () =
    (* Set up the save hook environment *)
    I.register_globals [
      "page_source", lua_str.embed page_source;
      "page_file", lua_str.embed env.page_file;
      "page_url", lua_str.embed env.page_url;
      "target_file", lua_str.embed env.target_file;
      "target_dir", lua_str.embed env.target_dir;
      "config", lua_of_toml hook_config;
      "hook_config", lua_of_toml hook_config;
      "soupault_config", lua_of_toml soupault_config;
      "force", I.Value.bool.embed settings.force;
      "build_dir", lua_str.embed settings.build_dir;
      "site_dir", lua_str.embed settings.site_dir
    ] state;
  in
  let () = Logs.info @@ fun m -> m "Running the save hook on page %s" env.page_file in
  Plugin_api.run_lua file_name state lua_code

let run_post_save_hook settings soupault_config hook_config file_name lua_code env =
  let open Defaults in
  let lua_str = I.Value.string in
  let state = I.mk () in
   let () =
    (* Set up the save hook environment *)
    I.register_globals [
      "page_file", lua_str.embed env.page_file;
      "page_url", lua_str.embed env.page_url;
      "target_file", lua_str.embed env.target_file;
      "target_dir", lua_str.embed env.target_dir;
      "config", lua_of_toml hook_config;
      "hook_config", lua_of_toml hook_config;
      "soupault_config", lua_of_toml soupault_config;
      "force", I.Value.bool.embed settings.force;
      "build_dir", lua_str.embed settings.build_dir;
      "site_dir", lua_str.embed settings.site_dir
    ] state;
  in
  let () = Logs.info @@ fun m -> m "Running the post-save hook on page %s" env.page_file in
  Plugin_api.run_lua file_name state lua_code

let run_pre_parse_hook settings soupault_config hook_config file_name lua_code page_file page_source =
  let open Defaults in
  let lua_str = I.Value.string in
  let state = I.mk () in
   let () =
    (* Set up the save hook environment *)
    I.register_globals [
      "page_source", lua_str.embed page_source;
      "page_file", lua_str.embed page_file;
      "config", lua_of_toml hook_config;
      "hook_config", lua_of_toml hook_config;
      "soupault_config", lua_of_toml soupault_config;
      "force", I.Value.bool.embed settings.force;
      "site_dir", lua_str.embed settings.site_dir
    ] state;
  in
  let (let*) = Result.bind in
  let () = Logs.info @@ fun m -> m "Running the pre-parse hook on page %s" page_file in
  let* () = Plugin_api.run_lua file_name state lua_code in
  let res = I.getglobal state (I.Value.string.embed "page_source") in
  if I.Value.string.is res then Ok (I.Value.string.project res)
  else Error "pre-parse hook has not assigned a string to the page_source variable"

let run_post_index_hook settings soupault_config hook_config file_name lua_code env soup fields =
  let assoc_of_json j =
    (* This function handles values projected from Lua,
       and Lua doesn't have a distinction between arrays/lists and tables:
       everything is a table.

       Thus if [fields] is empty, the projection function (Plugin_api.json_of_lua)
       can interpret it as either an empty table or an empty list,
       and both interpretations are valid.

       That's why we handle the empty list ([`A []]) case here. If [fields] is set to a non-empty list,
       it's clearly a logic bug, though.
     *)
    match j with
    | `O kvs -> kvs
    | `A [] -> []
    | _ -> internal_error @@ Printf.sprintf "post-index hook got a JSON value that isn't an object:\n %s"
      (Ezjsonm.value_to_string j)
  in
  let open Defaults in
  let lua_str = I.Value.string in
  let state = I.mk () in
   let () =
    (* Set up the post-index hook environment *)
    I.register_globals [
      "page", Plugin_api.lua_of_soup (Plugin_api.Html.SoupNode soup);
      "page_url", lua_str.embed env.page_url;
      "page_file", lua_str.embed env.page_file;
      "index_fields", Plugin_api.lua_of_json (`O fields);
      "config", lua_of_toml hook_config;
      "hook_config", lua_of_toml hook_config;
      "soupault_config", lua_of_toml soupault_config;
      "force", I.Value.bool.embed settings.force;
      "build_dir", lua_str.embed settings.build_dir;
      "site_dir", lua_str.embed settings.site_dir;
    ] state
  in
  let (let*) = Result.bind in
  let () = Logs.info @@ fun m -> m "Running the post-index hook on page %s" env.page_file in
  let* () = Plugin_api.run_lua file_name state lua_code in
  let index_fields = I.getglobal state (I.Value.string.embed "index_fields") in
  if not (I.Value.table.is index_fields) then
    Error "post-index hook has not assigned a table to the index_fields variable"
  else
    let* fields = Plugin_api.json_of_lua index_fields in
    Ok (assoc_of_json fields)

let run_pre_process_hook settings soupault_config hook_config file_name lua_code page_file target_dir target_file soup =
  let open Defaults in
  let lua_str = I.Value.string in
  let state = I.mk () in
   let () =
    (* Set up the post-index hook environment *)
    I.register_globals [
      "page", Plugin_api.lua_of_soup (Plugin_api.Html.SoupNode soup);
      "page_file", lua_str.embed page_file;
      "target_file", lua_str.embed target_file;
      "target_dir", lua_str.embed target_dir;
      "config", lua_of_toml hook_config;
      "hook_config", lua_of_toml hook_config;
      "soupault_config", lua_of_toml soupault_config;
      "force", I.Value.bool.embed settings.force;
      "build_dir", lua_str.embed settings.build_dir;
      "site_dir", lua_str.embed settings.site_dir
    ] state;
  in
  let (let*) = Result.bind in
  let () = Logs.info @@ fun m -> m "Running the pre-process hook on page %s" page_file in
  let* () = Plugin_api.run_lua file_name state lua_code in
  let* target_file = Plugin_api.get_global state "target_file" I.Value.string in
  let* target_dir = Plugin_api.get_global state "target_dir" I.Value.string in
  Ok (target_dir, target_file, soup)

let run_lua_index_processor soupault_config index_view_config file_name lua_code env soup =
  let page_from_lua p =
    let page_json = match Plugin_api.json_of_lua p with Ok p -> p | Error msg -> failwith msg in
    match page_json with
    | `O [("page_file", `String page_file);
          ("page_content", `String page_content)] ->
       let nav_path = Utils.split_path (FilePath.dirname page_file) |> Utils.drop_head in
       {page_file_path=page_file; page_content=(Some page_content); page_nav_path=nav_path}
    | _ ->
      failwith "generated page must be a table with fields \"page_file\" (string) and \"page_content\" (string)"
  in
  let open Defaults in
  let lua_str = I.Value.string in
  let table_list = I.Value.list I.Value.table in
  let state = I.mk () in
  let () =
    (* Set up the hook environment *)
    I.register_globals [
      "page", Plugin_api.lua_of_soup (Plugin_api.Html.SoupNode soup);
      "page_url", lua_str.embed env.page_url;
      "site_index", Plugin_api.lua_of_json (Utils.json_of_index_entries env.site_index);
      "page_file", lua_str.embed env.page_file;
      "target_file", lua_str.embed env.target_file;
      "target_dir", lua_str.embed env.target_dir;
      "config", lua_of_toml index_view_config;
      "index_view_config", lua_of_toml index_view_config;
      "soupault_config", lua_of_toml soupault_config;
      "force", I.Value.bool.embed env.settings.force;
      "build_dir", lua_str.embed env.settings.build_dir;
      "site_dir", lua_str.embed env.settings.site_dir
     ] state;
    (* Set the output variable `pages` to an empty list by default,
       so that index processors that don't create pagination or taxonomies
       don't have to set it at all.
     *)
    I.register_globals ["pages", table_list.embed []] state;
  in
  let (let*) = Result.bind in
  let () = Logs.info @@ fun m -> m "Running Lua index processor on page %s" env.page_file in
  let* () = Plugin_api.run_lua file_name state lua_code in
  let res = I.getglobal state (I.Value.string.embed "pages") in
  if not (table_list.is res) then Error "Index processor has not assigned a list of tables to the pages variable" else
  try Ok ((I.Value.list I.Value.value).project res |> List.map page_from_lua)
  with Failure msg -> Error (Printf.sprintf "Index processor generated a page incorrectly: %s" msg)

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
