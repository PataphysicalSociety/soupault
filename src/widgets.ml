open Soupault_common

module OH = Otoml.Helpers

open Defaults

exception Widget_error of string

type 'a widget = {
  config: Otoml.t;
  func: Defaults.state -> Otoml.t -> Defaults.index -> Defaults.page_data -> unit
}

(* The widgets datastructure is a widget priority list plus a hash with actual widgets *)
type 'a widgets = string list * (string, 'a widget) Hashtbl.t


(* Option monad *)
let (>>=) = Option.bind


(* Quick and dirty widget lookup *)
let find_widget plugins name =
  let plugin_w = Hashtbl.find_opt plugins name in
  let builtin_w = List.assoc_opt name Builtin_widgets.widgets in
  match plugin_w, builtin_w with
  | Some p, Some _ ->
    (* We intentionally allow users to replace built-in widgets with plugins,
       if they want to.
       It may create some confusion for people who try to use someone else's setup
       but I believe the benefits to flexibility outweigh that. *)
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

(* The underlying widget loading function.
   It raises [Widget_error] to escape from the loading loop,
   and thus shouldn't be used directly -- there's a result'y wrapper below it.
 *)
let rec _load_widgets settings soupault_config plugins ws hash =
  match ws with
  | [] -> ()
  | w :: ws' ->
    let widget_config = get_widget_config soupault_config w in
    let name = OH.find_string_opt widget_config ["widget"] in
    let fail msg = raise @@ Widget_error (Printf.sprintf "Error in [widgets.%s]: %s" w msg) in
    begin
      match name with
      | None -> fail {|missing required option widget="<some widget>"|}
      | Some name ->
        let widget_func = find_widget plugins name in
        begin
          match widget_func with
          | None ->
            (* It's not a built-in or an explicitly configured plugin. Try to find it in plugin directories. *)
            begin
              if not settings.plugin_discovery then
                let () = Logs.warn @@ fun m -> m {|Plugin discovery is disabled, not attempting to find a plugin that implements widget "%s"|} name in
                fail @@ Printf.sprintf {|unknown widget "%s"|} name
              else
                let file_name = Printf.sprintf "%s.lua" name in
                let file_path = Plugins.lookup_plugin_file settings.plugin_dirs file_name in
                match file_path with
                | None ->
                  let dirs_str = String.concat ", " settings.plugin_dirs in
                  let () = Logs.err @@ fun m -> m "Failed to find plugin file %s in directories [%s]" file_name dirs_str in
                  fail @@ Printf.sprintf {|widget "%s" is not a soupault built-in and is not provided by any available plugin|} name
                | Some plugin_file ->
                  let lua_source =
                    try Soup.read_file plugin_file
                    with Sys_error msg ->
                      fail @@ Printf.sprintf {|Could not read plugin file that provides widget "%s": %s|} name msg
                  in
                  let () = Hashtbl.add plugins name (Plugin_api.run_plugin name lua_source) in
                  let () = Logs.debug @@ fun m -> m "Widget %s is loaded from plugin file %s" name plugin_file in
                  let () = add_widget hash w (find_widget plugins name |> Option.get) widget_config in
                   _load_widgets settings soupault_config plugins ws' hash
            end
          | Some wf ->
            let () = add_widget hash w wf widget_config in
            _load_widgets settings soupault_config plugins ws' hash
        end
    end

(* Result'y wrapper for _load_widgets *)
let load_widgets settings soupault_config plugins =
  let () = Logs.info @@ fun m -> m "Mapping plugins to widgets" in
  let ws = list_widgets soupault_config in
  let widgets_hash = Hashtbl.create 1024 in
  try
    let () = _load_widgets settings soupault_config plugins ws widgets_hash in
    widgets_hash
  with Widget_error msg ->
    Printf.ksprintf soupault_error "Failed to load widgets: %s" msg

(* Widget dependency handling functions. *)

(* This type is only used inside this module,
   as a clean way to inject metadata extraction
   as a special kind of a dependency
   into the widget dependency graph. *)
type action = Widget of string | MetadataExtraction

let string_of_action action =
  match action with
  | Widget s -> s
  | MetadataExtraction ->
    (* XXX: We use this function to render widget names for debug purposes
       and to convert the topologically sorted list of actions
       to a list of widget names.

       This shouldn't be a problem because if indexing is disabled,
       MetadataExtraction will not be in the list at all;
       and if it's enabled, then the list will be split at that element.
       Thus it will never be added to any lists that we use to
       process widgets in a correct order.

       The fun fact is that
       "<metadata extraction>" is also a valid widget name,
       so there is a slight potential for confusion,
       if someone writes [widgets."<metadata extraction>"].

       But since TOML allows completely arbitrary keys,
       there is no way to produce a debug output in a way
       that unambiguously separates widget names from that virtual step.
     *)
    "<metadata extraction>"

(* For debug purposes. *)
let dump_dependency_graph dg =
  List.map (fun (k, v) -> Printf.sprintf "%s -> %s"
    (string_of_action k)
    (List.map string_of_action v |> String.concat ", ")) dg |> String.concat "\n"

(* Sorts the list of actions in a topological order.
   The list of actions may contain widgets
   and a special, unique, virtual step -- metadata extraction.

   One widget can be set to run only after another
   (the `after = ` option, so we need to sort them according
   to that option, at the very least. 

   Moreover, the user can tell soupault to extract page metadata
   only after processing certain widgets, using `index.extract_after_widgets`.

   That is to allow widgets to serve as metadata producers:
   a widget inserts something into the page (e.g., word count),
   then its output is added to the site index entry for that page.

   Metadata extraction certainly must run only
   after all widgets from the `index.extract_after_widgets` list
   are processed.

   However, we also need to ensure that metadata extraction
   runs as early as possible:
   immediately after all widgets from `index.extract_after_widgets`
   (and their own dependencies) are processed.

   Most widgets are expected to be consumers of the site index
   rather than producers, so placing metadata extraction
   later in the list of actions than it absolutely has to be
   would deprive those widgets of the opportunity to use the index.

   For this reason, we need to produce an extension
   of the widget dependency graph that includes
   metadata extraction as a new vertex
   and a bunch of new edges.

   One set of edges is explicit: those from `index.extract_after_widgets`.

   The other set is to express the idea
   that if a widget is not in `index.extract_after_widgets`
   and is not a dependency of anything that is,
   then it depends on the virtual `MetadataExtraction` action.

   The output either does not contain `MetadataExtraction`
   or it will be removed from the list
   by the partition_widgets function,
   so it will not cause widget lookup errors down the line.
  *)
let order_actions settings widget_hash =
  let () = Logs.info @@ fun m -> m "Determining widget processing order" in
  let format_bad_deps ds =
    let format_bad_dep (n, ns) =
      Printf.sprintf {|Widget "%s": is set to run after non-existent widgets: %s|}
        (string_of_action n) (String.concat ", " (List.map string_of_action ns))
    in
    let bad_deps = List.map format_bad_dep ds |> String.concat "\n" in
    Printf.sprintf "Found dependencies on non-existent widgets\n%s" bad_deps
  in
  let get_direct_deps widget =
    Config.find_strings_or ~default:[] widget.config ["after"] |> List.map (fun x -> Widget x)
  in
  (* Build the graph of direct widget dependencies
     (excluding dependencies related to the virtual step of metadata extraction).
   *)
  let direct_dep_graph = CCHashtbl.map_list
    (fun k v -> (Widget k, get_direct_deps v)) widget_hash
  in
  (* Build a list of all all dependencies of metadata extraction --
     direct and transitive.
   *)
  let index_deps = List.map (fun x -> Widget x) settings.index_extract_after_widgets in
  let index_deps =
    List.concat @@ List.map (Tsort.find_dependencies direct_dep_graph) index_deps |>
    List.append index_deps |>
    CCList.uniq ~eq:(=)
  in
  (* Add the metadata extraction step as a special kind of a build action,
     if indexing is enabled.
  *)
  let dep_graph =
    if settings.index then
      begin
        (* Add the dependency on metadata extraction to all widgets
           that are not in index.extract_after_widgets
           or in transitive dependencies of those widgets,
           so that widgets can benefit from access to the site index.
         *)
        let dep_graph = List.map
          (fun (name, deps) ->
            if not (Utils.in_list name index_deps)
            then (name, MetadataExtraction :: deps)
            else (name, deps))
          direct_dep_graph
        in
        (MetadataExtraction, index_deps) :: dep_graph
      end
    else direct_dep_graph
  in
  let () =
    Logs.debug @@ fun m -> m "Widget dependency graph:\n%s" (dump_dependency_graph dep_graph)
  in
  let bad_deps = Tsort.find_nonexistent_nodes dep_graph in
  if bad_deps <> [] then soupault_error (format_bad_deps bad_deps) else
  let res = Tsort.sort dep_graph in
  match res with
  | Tsort.Sorted ws -> ws
  | Tsort.ErrorCycle ws ->
    Printf.ksprintf soupault_error
      "There is a dependency cycle between following widgets: %s"
      (String.concat ", " (List.map string_of_action ws))

(* Splits the list of actions (widget calls and the special MetadataExtraction step)
   into those that should run before and after metadata extraction.

   The MetadataExtraction item, if it's there, is always removed from the list
   during that process.

   Assumes that the widget list list is already sorted in the topological order.
 *)
let partition_widgets sorted_action_list =
  let rec aux before_indexing remaining_actions =
    match remaining_actions with
    | [] ->
      (List.rev before_indexing, [])
    | action :: actions ->
      begin match action with
      | Widget _ as w -> aux (w :: before_indexing) actions
      | MetadataExtraction ->
        (List.rev before_indexing, actions)
      end
  in
  let before_indexing, after_indexing = aux [] sorted_action_list in
  (List.map string_of_action before_indexing,
   List.map string_of_action after_indexing)

let get_widgets settings soupault_config plugins index_deps =
  let widget_hash = load_widgets settings soupault_config plugins in
  let widget_order = order_actions settings widget_hash in
  if not settings.index then
    (* If indexing is disabled, there is no point in partitioning the widget order list --
       the whole concept of "widgets that need to run before metadata extraction"
       is moot in that case. *)
    begin
      let widget_order = List.map string_of_action widget_order in
      let () = Logs.debug @@ fun m -> m "Widget processing order: %s"
        (String.concat " " widget_order)
       in
       (widget_order, [], widget_hash)
    end
  else
    begin
      let before_index, after_index = partition_widgets widget_order in
      let () =
        if index_deps <> [] then begin
          Logs.debug @@ fun m -> m "Widgets that will run before metadata extraction: %s"
            (String.concat " " before_index);
          Logs.debug @@ fun m -> m "Widgets that will run after metadata extraction: %s"
            (String.concat " " after_index)
        end
      in
      (before_index, after_index, widget_hash)
    end

(** Check if a widget should run or not.

    If any of [exclude_page], [exclude_section], or [exclude_path_regex] options
    are present, they are checked first.
    If any of them matches, the page is excluded and the widget doesn't run on it.

    If any of [page], [section], or [path_regex] options are present,
    the widget runs if any of them matches.
    If not, the page is excluded.

    [page] and [section] options take relative paths, such as "section/page.html"
    or "section/subsection".
    [path_regex] is a Perl-compatible regex.

    If none of those options are present, the widget always runs.
 *)
let widget_should_run settings name widget page_file =
  let disabled = Config.find_bool_or ~default:false widget.config ["disabled"] in
  if disabled then
    let () = Logs.debug @@ fun m -> m {|Widget "%s" is disabled in the configuration|} name in false
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
      let () = Logs.debug @@ fun m -> m {|Widget "%s" will not run: page %s is excluded by its page/section/regex options|} name page_file in
      false
  end
