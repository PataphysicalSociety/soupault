open Defaults

module FU = FileUtil
module FP = FilePath

(* Result monad *)
let (>>=) = CCResult.(>>=)
let bind = CCResult.(>>=)
let return = CCResult.return

(*** Logging setup ***)

(* Omit the executable name from the logs, the user knows already *)
let pp_header ppf (l, h) =
  match h with
  | None -> if l = Logs.App then () else Format.fprintf ppf "[%a] " Logs.pp_level l
  | Some h -> Format.fprintf ppf "[%s] " h

let log_reporter = Logs.format_reporter ~pp_header:pp_header  ()

let setup_logging verbose debug =
  let level =
    if debug then Logs.Debug
    else if verbose then Logs.Info
    else Logs.Warning
  in
  Logs.set_level (Some level);
  Logs.set_reporter log_reporter

(*** Filesystem stuff ***)
let (+/) left right =
    FP.concat left right

let list_dirs path =
    FU.ls path |> FU.filter FU.Is_dir

let list_section_files settings path =
  let is_page_file f = Utils.in_list settings.page_extensions (FP.get_extension f) in
  let files = FU.ls path |> FU.filter (FU.Is_file) in
  let page_files = List.find_all is_page_file files in
  let other_files = List.find_all (fun f -> not (is_page_file f)) files in
  page_files, other_files

let make_build_dir build_dir =
  if (FU.test FU.Exists build_dir) then Ok () else
  let () = Logs.info @@ fun m -> m "Build directory \"%s\" does not exist, creating" build_dir in
  try
    let () = FU.mkdir build_dir in Ok ()
  with FileUtil.MkdirError e -> Error e

(** Creates a directory for the page if necessary.

    If clean URLs are used, then a subdirectory matching the page name
    is created inside the section directory, unless the page is
    a section index page.
    E.g. "site/foo.html" becomes "build/foo/index.html" to provide
    a clean URL.

    If clean URLs are not used, only section dirs are created.
 *)
let make_page_dir settings target_dir page_name =
  try
    (* Note: FileUtil.mkdir returns success if the directory
       already exists, this is why it's not checked *)
    let dir_name =
      if (page_name = settings.index_page) || (not settings.clean_urls) then target_dir
      else target_dir +/ page_name
    in
    FU.mkdir ~parent:true dir_name; Ok dir_name
  with FileUtil.MkdirError e -> Error e

let load_html settings file =
  let ext = FP.get_extension file in
  let preprocessor = CCList.assoc_opt ~eq:(=) ext settings.preprocessors in
  try
    match preprocessor with
    | None -> Ok (Soup.read_file file |> Soup.parse)
    | Some prep ->
      let prep_cmd = Printf.sprintf "%s %s" prep file in
      let () = Logs.info @@ fun m -> m "Calling preprocessor %s on page %s" prep file in
      Utils.get_program_output prep_cmd [| |] >>= (fun h -> Ok (Soup.parse h))
  with Sys_error e -> Error e

let save_html settings soup file =
  try
    let html_str = Soup.pretty_print soup in
    let chan = open_out file in
    (* lambdasoup doesn't include the doctype even if it was present
       in the source, so we have to do it ourselves *)
    settings.doctype |> String.trim |> Printf.fprintf chan "%s\n";
    Soup.write_channel chan html_str;
    close_out chan;
    Ok ()
  with Sys_error e -> Error e

let include_content settings html page_file =
  let content = load_html settings page_file in
  match content with
  | Ok c ->
    let element = Soup.select_one settings.content_selector html in
    begin
      match element with
      | Some element -> Ok (Soup.append_child element c)
      | None ->
        Error (Printf.sprintf "No element in the template matches selector \"%s\", nowhere to insert the content"
               settings.content_selector)
    end
  | Error _ as e -> e

(* Widget processing *)
let rec process_widgets settings env ws wh config soup =
  match ws with
  | [] -> Ok ()
  | w :: ws' ->
    begin
      let open Widgets in
      let widget = Hashtbl.find wh w in
      let () = Logs.info @@ fun m -> m "Processing widget %s on page %s" w env.page_file in
      if not (widget_should_run widget.config settings.site_dir env.page_file)
      then (process_widgets settings env ws' wh config soup) else
      let res = widget.func env widget.config soup in
      (* In non-strict mode, widget processing errors are tolerated *)
      match res, settings.strict with
      | Ok _, _ -> process_widgets settings env ws' wh config soup
      | Error _ as err, true -> err
      | Error msg, false ->
        let () = Logs.warn @@ fun m -> m "Processing widget \"%s\" failed: %s" w msg in
        process_widgets settings env ws' wh config soup
    end

(** Removes index page's parent dir from its navigation path

    When clean URLs are used, the "navigation path" as in the path
    before the page doesn'a match the "real" path for index pages,
    and if you try to use it for breadcrumbs for example,
    section index pages will have links to themselves,
    since the parent of foo/bar/index.html is technically "bar".
    The only way to deal with it I could find is to remove the
    last parent if the page is an index page.
 *)
let fix_nav_path settings path page_name =
  if page_name = settings.index_page then Utils.drop_tail path
  else path

let insert_index settings soup index =
  let index_container = Soup.select_one settings.index_selector soup in
  match index_container with
  | None -> Ok ()
  | Some ic ->
    begin
      match settings.index_processor with
      | None -> Ok (Autoindex.add_index settings ic index)
      | Some p ->
        let json = Autoindex.json_of_entries settings index in
        let () = Logs.info @@ fun m -> m "Calling index processor %s" p in
        let output = Utils.get_program_output ~input:(Some json) p [| |] in
        begin
          match output with
          | Error _ as e -> e
          | Ok output ->
            Ok (Soup.append_child ic (Soup.parse output))
        end
    end

let make_page_url settings nav_path page_file =
  let page =
    if settings.clean_urls then FP.basename page_file |> FP.chop_extension
    else FP.basename page_file
  in
  let path = List.append nav_path [page] in
  (* URL path should be absolute *)
  String.concat "/" path |> Printf.sprintf "/%s"

(** Processes a page:

    1. Adjusts the path to account for index vs non-index page difference
       in setups using clean URLs
    2. Reads a page file and inserts the content into the template
    3. Updates the global index if necessary
    4. Runs the page through widgets
    5. Inserts the index section into the page if it's an index page
    6. Saves the processes page to file
  *)
let process_page env index widgets config settings target_dir =
  let page_name = FP.basename env.page_file |> FP.chop_extension in
  (* If clean URLs are used, make_page_dir creates one,
     if not, just returns the current dir *)
  let%bind target_dir = make_page_dir settings target_dir page_name in
  let target_file =
    if settings.clean_urls then (target_dir +/ settings.index_file)
    (* If clean URLs aren't used, keep the original extension *)
    else target_dir +/ (FP.basename env.page_file)
  in
  let env = {env with nav_path = (fix_nav_path settings env.nav_path page_name)} in
  let () = Logs.info @@ fun m -> m "Processing page %s" env.page_file in
  let html = Soup.parse env.template in
  let%bind () = include_content settings html env.page_file in
  let widgets, widget_hash = widgets in
  let%bind () = process_widgets settings env widgets widget_hash config html in
  (* Section index injection *)
  let%bind () =
    if not settings.index then Ok () else
    let url = make_page_url settings env.nav_path env.page_file in
    (* Section index is inserted only into the index page *)
    if page_name <> settings.index_page
    then let () = index := (Autoindex.get_entry settings url env.nav_path html) :: !index  in Ok ()
    else insert_index settings html !index
  in
  let%bind () = save_html settings html target_file in
  Ok ()

(* Monadic wrapper for process_page that can either return or ignore errors  *)
let _process_page env index widgets config settings target_dir page_file =
    (* Make the page file name accessible to widgets *)
    let env = {env with page_file=page_file} in
    let res = process_page env index widgets config settings target_dir in
    match res with
      Ok _ -> Ok ()
    | Error msg ->
      let msg = Printf.sprintf "Could not process page %s: %s" page_file msg in
      if settings.strict then Error msg else 
      let () = Logs.warn @@ fun m -> m "%s" msg in
      Ok ()

(* Reorders the pages so that the index page is processed last,
   when the section index is available *)
let reorder_pages settings ps =
  let find_index = fun p -> (FP.basename p |> FP.chop_extension) = settings.index_page in
  let index_page = CCList.find_opt find_index ps in
  match index_page with
  | None -> ps
  | Some p ->
    let ps = CCList.remove ~eq:(=) ~key:p ps in
    List.append ps [p]

(** If index file path is configured, add section index to the global index
   that will be saved to the file *)
let save_index settings section_index index =
  match settings.dump_json with
  | None -> ()
  | Some _ -> index := List.append !section_index !index

(* Process the source directory recursively
   
 *)
let rec process_dir env index widgets config settings base_src_dir base_dst_dir dirname =
  let section_index = ref [] in
  let src_path = base_src_dir +/ dirname in
  let dst_path = base_dst_dir +/ dirname in
  let () = Logs.info @@ fun m -> m "Entering directory %s" src_path in
  let nav_path = if dirname <> "" then List.append env.nav_path [dirname] else env.nav_path in
  let env = {env with nav_path = nav_path} in
  let pages, assets = list_section_files settings src_path in
  let pages = reorder_pages settings pages in
  let () = FU.mkdir ~parent:true dst_path in
  let dirs = List.map (FP.basename) (list_dirs src_path) in
  let%bind () = Utils.iter (_process_page env section_index widgets config settings dst_path) pages in
  let%bind () = Utils.cp assets dst_path in
  let () = save_index settings section_index index in
  Utils.iter (process_dir env index widgets config settings src_path dst_path) dirs

(* Option parsing and initialization *)

let get_args settings =
  let init = ref false in
  let strict = ref settings.strict in
  let verbose = ref settings.verbose in
  let debug = ref settings.debug in
  let args = [
    ("--init", Arg.Unit (fun () -> init := true), "Setup basic directory structure");
    ("--verbose", Arg.Unit (fun () -> verbose := true), "Verbose output");
    ("--debug", Arg.Unit (fun () -> debug := true), "Debug output");
    ("--strict", Arg.Bool (fun s -> strict := s), "<true|false> Stop on page processing errors or not");
    ("--version", Arg.Unit (fun () -> Utils.print_version (); exit 0), "Print version and exit")
  ]
  in let usage = Printf.sprintf "Usage: %s [OPTIONS]" Sys.argv.(0) in
  let () = Arg.parse args (fun _ -> ()) usage in
  let settings = {settings with verbose = !verbose; debug = !debug; strict = !strict} in
  if !init then (Project_init.init settings; exit 0) else Ok settings

let check_project_dir settings =
  if (not (FU.test FU.Exists settings.default_template)) &&
     (not (FU.test FU.Is_dir settings.site_dir)) then
  Logs.warn @@ fun m -> m "Site directory %s and default template %s do not exist"
    settings.site_dir settings.default_template;
  Logs.warn @@ fun m -> m "Use %s --init to initialize a basic project" Sys.argv.(0);
  exit 1

let initialize () =
  let settings = Defaults.default_settings in
  let () = setup_logging settings.verbose settings.debug in
  let%bind config = Config.read_config Defaults.config_file in
  let settings = Config.update_settings settings config in
  let%bind settings = get_args settings in
  (* Update the log level from the config and arguments  *)
  let () = setup_logging settings.verbose settings.debug in
  let () = check_project_dir settings in
  let%bind plugins = Plugins.get_plugins config in
  let%bind widgets = Widgets.get_widgets config plugins in
  let%bind default_template_str = Utils.get_file_content settings.default_template in
  let default_env = {template=default_template_str; nav_path=[]; page_file=""} in
  Ok (config, widgets, settings, default_env)

let dump_index_json settings index =
  match settings.dump_json with
  | None -> Ok ()
  | Some f ->
    try Ok (Soup.write_file f @@ Autoindex.json_of_entries settings index)
    with Sys_error e -> Error e
  
let main () =
  let%bind config, widgets, settings, default_env = initialize () in
  let () = setup_logging settings.verbose settings.debug in
  let%bind () = make_build_dir settings.build_dir in
  let index = ref [] in
  let%bind () = process_dir default_env index widgets config settings settings.site_dir settings.build_dir "" in
  let%bind () = dump_index_json settings !index in
  return ()

let () =
  let res = main () in
  match res with
  | Ok _ -> exit 0
  | Error e ->
    Logs.err @@ fun m -> m "%s" e;
    exit 1

