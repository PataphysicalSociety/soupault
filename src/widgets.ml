type 'a widget = {
  config: TomlTypes.table;
  func: Defaults.env -> TomlTypes.table -> 'a Soup.node -> (unit, string) result
}

(* The widgets structure is widget priority list plus a hash with actual widgets *)
type 'a widgets = string list * (string, 'a widget) Hashtbl.t

(* Option monad *)
let (>>=) = CCOpt.(>>=)


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
  let widget_tbl = Config.get_table Defaults.widgets_table config >>= Config.get_table widget in
  match widget_tbl with
  | Some widget_tbl -> widget_tbl
  | None ->
    (* This function is, or should be used only with widget names already
       retrieved from the config *)
   failwith @@ Printf.sprintf "Trying to lookup a non-existent widget %s" widget

let list_widgets config =
  let ws = Config.get_table Defaults.widgets_table config >>= (fun x -> Some (Config.list_config_keys x)) in
  match ws with
  | None -> []
  | Some ws' -> ws'


(* The real widget loading function *)
let rec _load_widgets config plugins ws hash =
  match ws with
  | [] -> ()
  | w :: ws' ->
    let widget_config = get_widget_config config w in
    let name = Config.get_string "widget" widget_config in
    begin
      match name with
      | None -> failwith (Printf.sprintf "In [widgets.%s]: missing required option widget=\"<some widget>\"" w)
      | Some name ->
        let widget_func = find_widget plugins name in
        begin
          match widget_func with
          | None -> failwith (Printf.sprintf "In [widgets.%s]: unknown widget \"%s\"" w name)
          | Some wf ->
            let widget_rec = {config=widget_config; func=wf} in
            let () = Hashtbl.add hash w widget_rec in
            _load_widgets config plugins ws' hash
        end
    end

let get_widget_order hash =
  let dep_graph = CCHashtbl.map_list (fun k v -> (k, Config.get_strings_relaxed "after" v.config)) hash in
  let res = Tsort.sort dep_graph in
  match res with
  | Tsort.Sorted ws -> Ok ws
  | Tsort.ErrorCycle ws -> Error (Printf.sprintf "Found a circular dependency between widgets: %s" (String.concat " " ws))
  | Tsort.ErrorNonexistent ws -> Error (Printf.sprintf "Found dependencies on non-existent widgets: %s" (String.concat " " ws))

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
let load_widgets config plugins =
  let widgets_hash = Hashtbl.create 1024 in
  match config with
  | None -> Ok widgets_hash
  | Some config ->
    let ws = list_widgets config in
    try
      let () = _load_widgets config plugins ws widgets_hash in
      Ok widgets_hash
    with Failure msg -> Error msg

let get_widgets config plugins index_deps =
  let bind = CCResult.(>>=) in
  let%bind wh = load_widgets config plugins in
  let%bind wo = get_widget_order wh in
  let%bind before_index, after_index = partition_widgets wo index_deps in
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
let widget_should_run name widget site_dir page_file =
  let config = widget.config in
  let page_matches actual_path conf_path =
    let conf_path = FilePath.concat site_dir conf_path in
    (=) conf_path actual_path
  in
  let section_matches actual_path conf_path =
     (* Remove trailing slashes *)
     let conf_path = FilePath.concat site_dir conf_path |> Re.replace (Re.Perl.compile_pat "/+$") ~f:(fun _ -> "") in
     let page_dir = FilePath.dirname actual_path  in
     (* is_subdir doesn't consider a dir its own subdir,
        so we need to handle the same dir case explicitly.

        Moreover, it returns a false positive if the child matches the beginning but doesn't have a trailing slash,
        so here's this fixup.
       *)
     (FilePath.is_subdir conf_path (page_dir |> Printf.sprintf "%s/")) || (conf_path = page_dir)
  in
  let regex_matches actual_path path_re =
    let matches = Utils.get_matching_strings path_re actual_path in
    match matches with
    | Ok ms -> List.length ms <> 0
    | Error msg ->
      let () = Logs.warn @@ fun m -> m "Failed to check regex \"%s\" for widget %s (malformed regex?), assuming false: %s" path_re name msg in
      false
  in
  let pages = Config.get_strings_relaxed "page" config in
  let sections = Config.get_strings_relaxed "section" config in
  let regex = Config.get_strings_relaxed "path_regex" config in
  let pages_exclude = Config.get_strings_relaxed "exclude_page" config in
  let sections_exclude = Config.get_strings_relaxed "exclude_section" config in
  let regex_exclude = Config.get_strings_relaxed "exclude_path_regex" config in
  if (List.exists (regex_matches page_file) regex_exclude) ||
     (List.exists (page_matches page_file) pages_exclude)  ||
     (List.exists (section_matches page_file) sections_exclude)
  then let () = Logs.debug @@ fun m -> m "Page excluded from widget %s by a page/section/regex option" name in false
  else match pages, sections, regex with
  | [], [], [] -> true
  | _, _, _ ->
    let should_run =
      (List.exists (regex_matches page_file) regex) ||
      (List.exists (page_matches page_file) pages) ||
      (List.exists (section_matches page_file) sections)
    in
    let () =
      if not should_run then
      Logs.debug @@ fun m -> m "Page does not match any of the page/section/regex options, not running widget %s" name
    in should_run
