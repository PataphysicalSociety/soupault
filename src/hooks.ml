(* Hooks are extension points that allow the user to inject custom logic between page processing steps
   or take over a processing step with custom logic.
   They are configured in the [hooks] table.

   This module also hold the code for executing Lua index processors, which are configured differently
   but their internal execution process is very similar.

   To add a new hook:

   1. Add its name to [hook_types]
      It's important for config validation.
      NOTE: the order of the [hook_types] list doesn't matter,
            but it's better to keep it sorted in the hook execution order to avoid confusuion.
   2. Add a hook function to this module.
   3. Call that function from an appropriate place in src/soupault.ml or elsewhere.
 *)

open Common

module OH = Otoml.Helpers
module I = Plugin_api.I

(* The exhaustive list of valid hook names, for config validation purposes. *)
let hook_types = [
  "startup";
  "pre-parse";
  "pre-process";
  "post-index";
  "render";
  "save";
  "post-save";
  "post-build";
]

(* Auxilliary functions *)

exception Hook_error of string
let hook_error msg = raise (Hook_error msg)

let lua_of_toml = Plugin_api.lua_of_toml
let lua_of_json = Plugin_api.lua_of_json

(* Auxilliary functions *)

(* Checks if the user didn't try to add hooks of non-existent types.
   The set of hooks is fixed and their names must be from the [hook_types] list.
 *)
let check_hook_tables config =
  let hooks_table = Config.find_table_opt ["hooks"] config in
  match hooks_table with
  | None -> ()
  | Some tbl -> Config.check_subsections ~parent_path:["hooks"] tbl hook_types "hooks"

(* Checks if a hook should run on a specific page:
   it isn't disabled entirely for the current run
   and the page is not specifically excluded from it.
  *)
let hook_should_run settings hook_config hook_type page_file =
  let disabled = Config.find_bool_or ~default:false hook_config ["disabled"] in
  if disabled then
    let () = Logs.debug @@ fun m -> m "%s hook is disabled in the configuration" hook_type in
    false
  else
    let options = Config.get_path_options hook_config in
    let profile = OH.find_string_opt hook_config ["profile"] in
    if not (Utils.build_profile_matches profile settings.build_profiles) then
      let () = Logs.debug @@ fun m -> m "%s hook is not used: not enabled in the current build profile (%s)"
        hook_type (Option.value ~default:"default" profile)
      in false
    else begin
      if Path_options.page_included settings options settings.site_dir page_file then true
      else
        let () = Logs.debug @@ fun m -> m "%s hook is not used: page %s is excluded by its page/section/regex options"
          hook_type page_file
        in
        false
    end

(* Loads hook code from an inline snippet in the config or from a file. *)
let load_hook hook_config ident =
  let default_filename = Printf.sprintf {|<inline Lua source for hook "%s">|} ident in
  let ident = Printf.sprintf {|hook "%s"|} ident in
  let res = Utils.load_plugin_code hook_config default_filename ident in
  match res with
  | Ok (file_name, source_code) -> (file_name, source_code)
  | Error msg -> config_error msg

(* Loads a single hook from its configuration. *)
let get_hook config hooks_hash ident =
  let hook_config = Config.find_table_opt ["hooks"; ident] config in
  match hook_config with
  | None -> ()
  | Some hook_config ->
    let (file_name, source) = load_hook hook_config ident in
    Hashtbl.add hooks_hash ident (file_name, source, hook_config)

(* Loads all hooks from the config. *)
let get_hooks config =
  let () = Logs.info @@ fun m -> m "Loading hooks" in
  let () = check_hook_tables config in
  let hooks_hash = Hashtbl.create 1024 in
  let () = List.iter (get_hook config hooks_hash) hook_types in
  hooks_hash

(* Hook functions *)

(* A helper function to get the index entry for the current page from the index hash *)
let get_index_entry_json site_index page_file =
  let index_entry =
    List.find_opt (fun e -> e.index_entry_page_file = page_file) site_index
  in
  match index_entry with
  | None -> `Null
  | Some ie -> Utils.json_of_index_entry ie

(* The pre-parse hook runs just after the page source is loaded from a file
   or received from a preprocessor
   and before it's parsed into an HTML element tree.

   It only has access to the page source, not an element tree.
   It can modify the [page_source] variable that contains the page source.
 *)
let run_pre_parse_hook soupault_state hook_config file_name lua_code page_file page_source =
  let lua_str = I.Value.string in
  let lua_state = I.mk () in
  let settings = soupault_state.soupault_settings in
   let () =
    (* Set up the hook environment *)
    I.register_globals [
      "page_source", lua_str.embed page_source;
      "page_file", lua_str.embed page_file;
      "config", lua_of_toml hook_config;
      "hook_config", lua_of_toml hook_config;
      "soupault_config", lua_of_toml soupault_state.soupault_config;
      "force", I.Value.bool.embed settings.force;
      "site_dir", lua_str.embed settings.site_dir;
    ] lua_state
  in
  let () = Logs.info @@ fun m -> m "Running the pre-parse hook on page %s" page_file in
  let () =
    try Plugin_api.run_lua lua_state file_name lua_code
    with Plugin_error msg ->
      Printf.ksprintf soupault_error
        "Failed to run pre-parse hook on page %s: %s" page_file msg
  in
  let res = I.getglobal lua_state (I.Value.string.embed "page_source") in
  if I.Value.string.is res then I.Value.string.project res
  else Printf.ksprintf soupault_error
    "Failed to run the pre-parse hook on page %s:\
       hook has not assigned a string to the page_source variable"
    page_file

(* The pre-process hook runs just after a page source is parsed into an HTML element tree
   and before soupault itself does anything with it (before any widgets run).

   It has access to the page element tree and can modify it.
 *)
let run_pre_process_hook soupault_state hook_config file_name lua_code page_file target_dir target_file soup =
  let lua_str = I.Value.string in
  let lua_state = I.mk () in
  let settings = soupault_state.soupault_settings in
   let () =
    (* Set up the post-index hook environment *)
    I.register_globals [
      "page", Plugin_api.lua_of_soup (Plugin_api.Html.SoupNode soup);
      "page_file", lua_str.embed page_file;
      "target_file", lua_str.embed target_file;
      "target_dir", lua_str.embed target_dir;
      "config", lua_of_toml hook_config;
      "hook_config", lua_of_toml hook_config;
      "soupault_config", lua_of_toml soupault_state.soupault_config;
      "force", I.Value.bool.embed settings.force;
      "build_dir", lua_str.embed settings.build_dir;
      "site_dir", lua_str.embed settings.site_dir;
    ] lua_state
  in
  let () = Logs.info @@ fun m -> m "Running the pre-process hook on page %s" page_file in
  let () =
    try Plugin_api.run_lua lua_state file_name lua_code
    with Plugin_error msg ->
      Printf.ksprintf soupault_error
        "Failed to run the pre-process hook on page %s: %s" page_file msg
  in
  let target_file = Plugin_api.get_global lua_state "target_file" I.Value.string in
  let target_dir = Plugin_api.get_global lua_state "target_dir" I.Value.string in
  (target_dir, target_file, soup)

(* The post-index hook runs after soupault extracts index fields from a page.

   It has access to the page element tree and also to extracted index fields
   and can modify both.
 *)
let run_post_index_hook soupault_state hook_config file_name lua_code page entry =
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
  let lua_str = I.Value.string in
  let lua_state = I.mk () in
  let settings = soupault_state.soupault_settings in
  let soup = page.element_tree in
  let index_entry_json =  Utils.json_of_index_entry entry in
  let () =
    (* Set up the post-index hook environment *)
    I.register_globals [
      "page", Plugin_api.lua_of_soup (Plugin_api.Html.SoupNode soup);
      "page_url", lua_str.embed page.url;
      "page_file", lua_str.embed page.page_file;
      "index_entry", Plugin_api.lua_of_json index_entry_json;
      "index_fields", Plugin_api.lua_of_json (`O entry.fields);
      "config", lua_of_toml hook_config;
      "hook_config", lua_of_toml hook_config;
      "soupault_config", lua_of_toml soupault_state.soupault_config;
      "force", I.Value.bool.embed settings.force;
      "build_dir", lua_str.embed settings.build_dir;
      "site_dir", lua_str.embed settings.site_dir;
      "ignore_page", I.Value.bool.embed false;
    ] lua_state
  in
  let () = Logs.info @@ fun m -> m "Running the post-index hook on page %s" page.page_file in
  let () =
    try Plugin_api.run_lua lua_state file_name lua_code
    with Plugin_error msg ->
      Printf.ksprintf soupault_error
        "Failed to run the post-index hook on page %s: %s" page.page_file msg
  in
  (* XXX: The assumption is that there's no way to completely unset a global
          in the Lua interpreter we are using,
          so if we added [ignore_page] to globals, retrieving it will never cause errors,
          and that projection to a bool will never fail either.
   *)
  let index_fields = I.getglobal lua_state (I.Value.string.embed "index_fields") in
  if not (I.Value.table.is index_fields) then
    Printf.ksprintf soupault_error
      "Failed to run the post-index hook on page %s:\
         hook has not assigned a table to the index_fields variable"
      page.page_file
  else
    let fields = Plugin_api.json_of_lua index_fields in
    assoc_of_json fields

(* The render hook replaces the normal page rendering process.

   It must assign HTML source code generated from the element tree
   to the [page_source] variable.
 *)
let run_render_hook soupault_state hook_config file_name lua_code page =
  let lua_str = I.Value.string in
  let lua_state = I.mk () in
  let settings = soupault_state.soupault_settings in
  let index_entry_json = get_index_entry_json soupault_state.site_index page.page_file in
  let soup = page.element_tree in
  let () =
    (* Set up the hook environment *)
    I.register_globals [
      "page", Plugin_api.lua_of_soup (Plugin_api.Html.SoupNode soup);
      "page_file", lua_str.embed page.page_file;
      "page_url", lua_str.embed page.url;
      "site_index", Plugin_api.lua_of_json (Utils.json_of_index_entries soupault_state.site_index);
      "index_entry", Plugin_api.lua_of_json index_entry_json;
      "target_file", lua_str.embed page.target_file;
      "target_dir", lua_str.embed page.target_dir;
      "config", lua_of_toml hook_config;
      "hook_config", lua_of_toml hook_config;
      "soupault_config", lua_of_toml soupault_state.soupault_config;
      "force", I.Value.bool.embed settings.force;
      "build_dir", lua_str.embed settings.build_dir;
      "site_dir", lua_str.embed settings.site_dir;
    ] lua_state
  in
  let () = Logs.info @@ fun m -> m "Running the render hook on page %s" page.page_file in
  let () =
    try Plugin_api.run_lua lua_state file_name lua_code
    with Plugin_error msg ->
      Printf.ksprintf soupault_error
        "Failed to run the render hook on page %s: %s" page.page_file msg
  in
  let res = I.getglobal lua_state (I.Value.string.embed "page_source") in
  if I.Value.string.is res then I.Value.string.project res
  else Printf.ksprintf soupault_error
    "Failed to run the render hook on page %s: \
       hook has not assigned a string to the page_source variable"
    page.page_file

let run_save_hook soupault_state hook_config file_name lua_code page page_source =
  let lua_str = I.Value.string in
  let lua_state = I.mk () in
  let index_entry_json = get_index_entry_json soupault_state.site_index page.page_file in
  let settings = soupault_state.soupault_settings in
  let () =
    (* Set up the hook environment *)
    I.register_globals [
      "page_source", lua_str.embed page_source;
      "page_file", lua_str.embed page.page_file;
      "page_url", lua_str.embed page.url;
      "site_index", Plugin_api.lua_of_json (Utils.json_of_index_entries soupault_state.site_index);
      "index_entry", Plugin_api.lua_of_json index_entry_json;
      "target_file", lua_str.embed page.target_file;
      "target_dir", lua_str.embed page.target_dir;
      "config", lua_of_toml hook_config;
      "hook_config", lua_of_toml hook_config;
      "soupault_config", lua_of_toml soupault_state.soupault_config;
      "force", I.Value.bool.embed settings.force;
      "build_dir", lua_str.embed settings.build_dir;
      "site_dir", lua_str.embed settings.site_dir;
    ] lua_state
  in
  let () = Logs.info @@ fun m -> m "Running the save hook on page %s" page.page_file in
  try Plugin_api.run_lua lua_state file_name lua_code
  with Plugin_error msg ->
    Printf.ksprintf soupault_error
      "Failed to run the save hook on page %s: %s" page.page_file msg

(*  These two hooks are executed only once rather than for every page. *)

(* The startup hook runs on soupault startup, before it starts processing pages.

   This hook can set [global_data] variable that other plugins and hooks
   can then read using the [Plugin.get_global_data] function.
 *)
let run_startup_hook soupault_state hooks =
  (* Import global data from the Lua state that the hook might have set. *)
  let import_global_data gdt =
    (* If the hook did not set global_data or set it to [nil], just do nothing. *)
    if (I.Value.unit.is gdt) then () else
    (* If it's set to a non-[nil] value, it must be a table. *)
    if not (I.Value.table.is gdt) then Printf.ksprintf soupault_error
      "Failed to run the startup hook: \
         hook assigned a value of a wrong type to the global_data variable: \
         expected a table, found %s" (I.Value.to_string gdt)
    else let hash = I.Value.table.project gdt in
    I.Value.Luahash.iter
      (fun k v ->
        if I.Value.string.is k
        then Hashtbl.add Common.global_data
          (I.Value.string.project k) (Plugin_api.json_of_lua v)
        else Printf.ksprintf soupault_error
          "startup hook used a wrong key type (%s) in global_data"
          (I.Value.to_string k))
      hash
  in
  let hook = Hashtbl.find_opt hooks "startup" in
  match hook with
  | None -> ()
  | Some (file_name, source_code, hook_config) ->
    let lua_str = I.Value.string in
    let lua_state = I.mk () in
    let settings = soupault_state.soupault_settings in
    let () =
      (* Set up the hook environment *)
      I.register_globals [
        "config", lua_of_toml hook_config;
        "hook_config", lua_of_toml hook_config;
        "soupault_config", lua_of_toml soupault_state.soupault_config;
        "force", I.Value.bool.embed settings.force;
        "site_dir", lua_str.embed settings.site_dir;
      ] lua_state
    in
    let () = Logs.info @@ fun m -> m "Running the startup hook" in
    let () =
      try Plugin_api.run_lua lua_state file_name source_code
      with Plugin_error msg ->
        Printf.ksprintf soupault_error "Failed to run startup hook: %s" msg
    in
    let res = I.getglobal lua_state (I.Value.string.embed "global_data") in
    import_global_data res

(* The post-build hook runs when soupault has finished saving generated pages to disk,
   just before it exits.
 *)
let run_post_build_hook soupault_state site_index hooks =
  let hook = Hashtbl.find_opt hooks "post-build" in
  match hook with
  | None -> ()
  | Some (file_name, source_code, hook_config) ->
    let lua_str = I.Value.string in
    let lua_state = I.mk () in
    let settings = soupault_state.soupault_settings in
    let () =
      (* Set up the hook environment *)
      I.register_globals [
        "config", lua_of_toml hook_config;
        "hook_config", lua_of_toml hook_config;
        "soupault_config", lua_of_toml soupault_state.soupault_config;
        "force", I.Value.bool.embed settings.force;
        "build_dir", lua_str.embed settings.build_dir;
        "site_dir", lua_str.embed settings.site_dir;
        "site_index", Plugin_api.lua_of_json (Utils.json_of_index_entries site_index);
      ] lua_state
    in
    let () = Logs.info @@ fun m -> m "Running the post-build hook" in
    (* Since this hook runs just before soupault finishes its work and exits,
       there's no reason to update the global data variable --
       there's no plugin code left to run that could use it at that point.
     *)
    try Plugin_api.run_lua lua_state file_name source_code
    with Plugin_error msg ->
      Printf.ksprintf soupault_error
        "Failed to run the post-build hook: %s" msg

(* Lua index processors aren't actually hooks but their execution process is similar. *)

let run_lua_index_processor soupault_state index_view_config view_name file_name lua_code page =
  let page_from_lua p =
    match (Plugin_api.json_of_lua p) with
    | `O [("page_file", `String page_file);
          ("page_content", `String page_content)] ->
       let soup = Html_utils.parse_page ~fragment:true soupault_state.soupault_settings page_content in
       (page_file, soup)
    | _ | exception Plugin_error _ ->
      hook_error {|generated page must be a table with fields "page_file" (string) and "page_content" (string)|}
  in
  let lua_str = I.Value.string in
  let table_list = I.Value.list I.Value.table in
  let lua_state = I.mk () in
  let index_entry_json = get_index_entry_json soupault_state.site_index page.page_file in
  let settings = soupault_state.soupault_settings in
  let () =
    (* Set up index processor environment *)
    I.register_globals [
      "page", Plugin_api.lua_of_soup (Plugin_api.Html.SoupNode page.element_tree);
      "page_url", lua_str.embed page.url;
      "site_index", Plugin_api.lua_of_json (Utils.json_of_index_entries soupault_state.site_index);
      "index_entry", Plugin_api.lua_of_json index_entry_json;
      "page_file", lua_str.embed page.page_file;
      "target_file", lua_str.embed page.target_file;
      "target_dir", lua_str.embed page.target_dir;
      "config", lua_of_toml index_view_config;
      "index_view_config", lua_of_toml index_view_config;
      "soupault_config", lua_of_toml soupault_state.soupault_config;
      "force", I.Value.bool.embed settings.force;
      "build_dir", lua_str.embed settings.build_dir;
      "site_dir", lua_str.embed settings.site_dir;
     ] lua_state;
    (* Set the output variable [pages] to an empty list by default,
       so that index processors that don't create pagination or taxonomies
       don't have to set it at all.
     *)
    I.register_globals ["pages", table_list.embed []] lua_state;
  in
  let () = Logs.info @@ fun m -> m {|Running Lua index processor %s for index view "%s" on page %s|}
    file_name view_name page.page_file
  in
  let () =
    try Plugin_api.run_lua lua_state file_name lua_code
    with Plugin_error msg ->
      Printf.ksprintf soupault_error
        "Failed to run a Lua index processor for view %s on page %s: %s" view_name page.page_file msg
  in
  let res = I.getglobal lua_state (I.Value.string.embed "pages") in
  if not (table_list.is res)
  then Printf.ksprintf soupault_error
    "Index processor error for view %s on page %s has assigned a list of tables to the pages variable"
    view_name page.page_file
  else
    try (I.Value.list I.Value.value).project res |> List.map page_from_lua
    with Hook_error msg ->
      Printf.ksprintf soupault_error
        "Index processor for view %s on page %s generated a page incorrectly: %s"
       view_name page.page_file msg
