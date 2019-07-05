type 'a widget = {
  name : string;
  config: TomlTypes.table;
  func: Defaults.env -> TomlTypes.table -> 'a Soup.node -> (unit, string) result
}

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
  let (>>=) = CCOpt.(>>=) in
  let ws = Config.get_table Defaults.widgets_table config >>= (fun x -> Some (Config.list_config_keys x)) in
  match ws with
  | None -> []
  | Some ws' -> ws'

(* The real widget loading function *)
let rec _load_widgets config ws =
  match ws with
  | [] -> []
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
            let widget_rec = {name=n; config=widget_config; func=wf} in
            widget_rec :: (_load_widgets config ws')
        end
    end

(* The monadic wrapping for it *)
let load_widgets config =
  let ws = list_widgets config in
  (* Keep widget order the same as in the config *)
  let ws = List.rev ws in
  try Ok (_load_widgets config ws)
  with Failure msg -> Error msg

(** Check if a widget should run or not.

    There are two options for it: page= and section=
    They are paths relative to the $site_dir, e.g.
    page = "articles/theorems-for-free.html"

    If both are present, then page path is checked first,
    if it doesn't match, then the section path is checked.

    If an option is absent, it means the widget doesn't need
    that condition to run. If both options are absent,
    the widget will run on all pages.
 *)
let should_widget_run config site_dir page_file =
  let page_matches conf_path actual_path =
    let conf_path = FilePath.concat site_dir conf_path in
    (=) conf_path actual_path
  in
  let section_matches conf_path actual_path =
     let conf_path = FilePath.concat site_dir conf_path in
     let page_dir = FilePath.dirname actual_path in
     FilePath.is_subdir conf_path page_dir
  in
  let page = Config.get_string "page" config in
  let section = Config.get_string "section" config in
  match page, section with
  | None, None -> true
  | Some p, None -> page_matches p page_file
  | None, Some s -> section_matches s page_file
  | Some p, Some s ->
    if page_matches p page_file then true
    else section_matches s page_file
