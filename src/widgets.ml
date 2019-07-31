type 'a widget = {
  config: TomlTypes.table;
  func: Defaults.env -> TomlTypes.table -> 'a Soup.node -> (unit, string) result
}

(* The widgets structure is widget priority list plus a hash with actual widgets *)
type 'a widgets = string list * (string, 'a widget) Hashtbl.t

(* Option monad *)
let (>>=) = CCOpt.(>>=)


(* Quick and dirty widget lookup *)
let find_widget name =
  try
    Some (List.assoc name Builtin_widgets.widgets)
  with Not_found -> None


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
let rec _load_widgets config ws hash =
  match ws with
  | [] -> ()
  | w :: ws' ->
    let widget_config = get_widget_config config w in
    let name = Config.get_string "widget" widget_config in
    begin
      match name with
      | None -> failwith (Printf.sprintf "In [widgets.%s]: missing required option widget=\"<some widget>\"" w)
      | Some n ->
        let widget_func = find_widget n in
        begin
          match widget_func with
          | None -> failwith (Printf.sprintf "In [widgets.%s]: unknown widget \"%s\"" w n)
          | Some wf ->
            let widget_rec = {config=widget_config; func=wf} in
            let () = Hashtbl.add hash w widget_rec in
            _load_widgets config ws' hash
        end
    end

let get_widget_order hash =
  let dep_graph = CCHashtbl.map_list (fun k v -> (k, Config.get_strings_relaxed "after" v.config)) hash in
  Tsort.sort dep_graph

(* The monadic wrapping for it *)
let load_widgets config =
  let widgets_hash = Hashtbl.create 1024 in
  match config with
  | None -> Ok widgets_hash
  | Some config ->
    let ws = list_widgets config in
    try
      let () = _load_widgets config ws widgets_hash in
      Ok widgets_hash
    with Failure msg -> Error msg

let get_widgets config =
  let bind = CCResult.(>>=) in
  let%m wh = load_widgets config in
  let%m wo = get_widget_order wh in
  Ok (wo, wh)

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
let widget_should_run config site_dir page_file =
  let page_matches actual_path conf_path =
    let conf_path = FilePath.concat site_dir conf_path in
    (=) conf_path actual_path
  in
  let section_matches actual_path conf_path =
     let conf_path = FilePath.concat site_dir conf_path in
     let page_dir = FilePath.dirname actual_path in
     FilePath.is_subdir conf_path page_dir
  in
  let regex_matches actual_path path_re =
    let matches = Utils.get_matching_strings path_re actual_path in
    match matches with
    | Ok ms -> List.length ms <> 0
    | Error msg ->
      let () = Logs.warn @@ fun m -> m "Could not check regex, assuming false: %s" msg in
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
  then let () = Logs.info @@ fun m -> m "Page excluded, not running the widget" in false
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
      Logs.info @@ fun m -> m "Page doesn't match any options, not running the widget"
    in should_run
