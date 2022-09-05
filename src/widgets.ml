open Soupault_common

module OH = Otoml.Helpers

open Defaults

type 'a widget = {
  config: Otoml.t;
  func: Defaults.env -> Otoml.t -> 'a Soup.node -> (unit, string) result
}

(* The widgets structure is widget priority list plus a hash with actual widgets *)
type 'a widgets = string list * (string, 'a widget) Hashtbl.t

(* Option monad *)
let (>>=) = Option.bind


(* Quick and dirty widget lookup *)
let find_widget plugins name =
  let plugin_w = Hashtbl.find_opt plugins name in
  let builtin_w = List.assoc_opt name Builtin_widgets.widgets in
  match plugin_w, builtin_w with
  | Some p, Some _ ->
    let () = Logs.warn @@ fun m -> m "Widget name %s is redefined by a plugin" name in
    Some p
  | Some p, None -> Some p
  | None, Some b -> Some b
  | _ -> None

(* Widget config loading *)
let get_widget_config config widget =
  let widget_tbl = Config.find_table_opt [Defaults.widgets_table; widget] config in
  match widget_tbl with
  | Some widget_tbl -> widget_tbl
  | None ->
    (* This function is (or should be) used only with widget names retrieved from the config.
       If any code tries to look up a widget name that isn't defined in the config,
       it means there's a bug.
     *)
   internal_error @@ Printf.sprintf "Trying to lookup a non-existent widget %s" widget

let list_widgets config =
  let ws = Config.find_table_opt [Defaults.widgets_table] config >>= (fun x -> Some (Otoml.list_table_keys x)) in
  match ws with
  | None -> []
  | Some ws' -> ws'

let add_widget hash name widget_func widget_config =
  let widget_rec = {config=widget_config; func=widget_func} in
  Hashtbl.add hash name widget_rec

(* The real widget loading function *)
let rec _load_widgets settings config plugins ws hash =
  match ws with
  | [] -> ()
  | w :: ws' ->
    let widget_config = get_widget_config config w in
    let name = OH.find_string_opt widget_config ["widget"] in
    let fail msg = Printf.ksprintf failwith "Error in [widgets.%s]: %s" w msg in
    begin
      match name with
      | None -> fail "missing required option widget=\"<some widget>\""
      | Some name ->
        let widget_func = find_widget plugins name in
        begin
          match widget_func with
          | None ->
            (* It's not a built-in or an explicitly configured plugin. Try to find it in plugin directories. *)
            begin
              if not settings.plugin_discovery then
                let () = Logs.warn @@ fun m -> m "Plugin discovery is disabled, not attempting to find a plugin that implements widget \"%s\"" name in
                fail @@ Printf.sprintf "unknown widget \"%s\"" name
              else
                let file_name = Printf.sprintf "%s.lua" name in
                let file_path = Plugins.lookup_plugin_file settings.plugin_dirs file_name in
                match file_path with
                | None ->
                  let dirs_str = String.concat ", " settings.plugin_dirs in
                  let () = Logs.err @@ fun m -> m "Failed to find plugin file %s in directories [%s]" file_name dirs_str in
                  fail @@ Printf.sprintf "widget \"%s\" is not a soupault built-in and is not provided by any available plugin" name
                | Some plugin_file ->
                  let lua_source =
                    try Soup.read_file plugin_file
                    with Sys_error msg ->
                      fail @@ Printf.sprintf "Could not read plugin file that provides widget \"%s\": %s" name msg
                  in
                  let () = Hashtbl.add plugins name (Plugins.make_plugin_function lua_source settings config name) in
                  let () = Logs.debug @@ fun m -> m "Widget %s is loaded from plugin file %s" name plugin_file in
                  let () = add_widget hash w (find_widget plugins name |> Option.get) widget_config in
                   _load_widgets settings config plugins ws' hash
            end
          | Some wf ->
            let () = add_widget hash w wf widget_config in
            _load_widgets settings config plugins ws' hash
        end
    end


let get_widget_order hash =
  let format_bad_deps ds =
    let format_bad_dep (n, ns) = Printf.sprintf "Widget \"%s\" depends on non-existent widgets: %s" n (String.concat ", " ns) in
    let bad_deps = List.map format_bad_dep ds |> String.concat "\n" in
    Printf.sprintf "Found dependencies on non-existent widgets\n%s" bad_deps
  in
  let dep_graph = CCHashtbl.map_list (fun k v -> (k, Config.find_strings_or ~default:[] v.config ["after"])) hash in
  let bad_deps = Tsort.find_nonexistent_nodes dep_graph in
  if bad_deps <> [] then Error (format_bad_deps bad_deps) else
  let res = Tsort.sort dep_graph in
  match res with
  | Tsort.Sorted ws -> Ok ws
  | Tsort.ErrorCycle ws -> Error (Printf.sprintf "Found a circular dependency between widgets: %s" (String.concat " " ws))

(** Splits the ordered list of widgets into parts that should run before and after index extraction.

    
 *)
let partition_widgets all_widgets index_deps =
  let rec aux index_deps before_index after_index =
    match index_deps, after_index with
    (* All dependencies are removed, nothing else to do *)
    | [], ws -> Ok (List.rev before_index, ws)
    (* There are still dependencies to remove *)
    | _, w :: ws' ->
      let index_deps = CCList.remove ~eq:(=) ~key:w index_deps in
      aux index_deps (w :: before_index) ws'
    (* The list or widgets is empty, but the list is dependencies is not,
       that means index extraction depends on widgets that don't exist
       in the config *)
    | _ as ds, []  -> Error (Printf.sprintf "Index extraction depends on non-existent widgets: %s" (String.concat " " ds))
  in aux index_deps [] all_widgets


(* The monadic wrapping for it *)
let load_widgets settings config plugins =
  let widgets_hash = Hashtbl.create 1024 in
  match config with
  | None -> Ok widgets_hash
  | Some config ->
    let ws = list_widgets config in
    try
      let () = _load_widgets settings config plugins ws widgets_hash in
      Ok widgets_hash
    with Failure msg -> Error msg

let get_widgets settings config plugins index_deps =
  let (let*) = Stdlib.Result.bind in
  let* wh = load_widgets settings config plugins in
  let* wo = get_widget_order wh in
  let* before_index, after_index = partition_widgets wo index_deps in
  let () =
    Logs.debug @@ fun m -> m "Widget processing order: %s" (String.concat " " wo);
    if index_deps <> [] then begin
      Logs.debug @@ fun m -> m "Widgets that will run before metadata extraction: %s" (String.concat " " before_index);
      Logs.debug @@ fun m -> m "Widgets that will run after metadata extraction: %s" (String.concat " " after_index)
    end
  in
  Ok (before_index, after_index, wh)

(** Check if a widget should run or not.

    If any of exlude_page, exclude_section, or exclude_path_regex options
    are present, they are checked first.
    If any of them matches, page is excluded and widget doesn't run.

    If any of page, section, or path_regex options are present,
    the widget runs if any of them matches.
    If not, page is excluded.

    page and section options take relative paths, such as "section/page.html"
    or "section/subsection".
    path_regex is a Perl-compatible regex.

    If none of those options are present, widget always runs.
 *)
let widget_should_run settings name widget page_file =
  let disabled = Config.find_bool_or ~default:false widget.config ["disabled"] in
  if disabled then
    let () = Logs.debug @@ fun m -> m "Widget \"%s\" is disabled in the configuration" name in false
  else
  let options = Config.get_path_options widget.config in
  let profile = OH.find_string_opt widget.config ["profile"] in
  if not (Utils.build_profile_matches profile settings.build_profiles) then
    let () = Logs.debug @@ fun m -> m {|Widget "%s" is not enabled in the current build profile (%s)|}
      name (Option.value ~default:"default" profile)
    in false
  else begin
    if Path_options.page_included settings options settings.site_dir page_file then true
    else
      let () = Logs.debug @@ fun m -> m "Widget \"%s\" will not run: page %s is excluded by its page/section/regex options" name page_file in
      false
  end
