open Defaults

module FU = FileUtil
module FP = FilePath

(* Result monad *)
let bind = CCResult.(>>=)
let return = CCResult.return

(* Logging setup *)
let setup_logging verbose =
  let level = if verbose then Logs.Info else Logs.Warning in
  Logs.set_level (Some level);
  Logs.set_reporter (Logs_fmt.reporter ())

(* Filesystem stuff *)
let (+/) left right =
    FP.concat left right

let list_dirs path =
    FU.ls path |> FU.filter FU.Is_dir

let list_page_files path =
    FU.ls path |> FU.filter (FU.Is_file)

let make_build_dir build_dir =
  let () = Logs.info @@ fun m -> m "Build directory \"%s\" does not exist, creating" build_dir in
  try
    let () = FU.mkdir build_dir in Ok ()
  with FileUtil.MkdirError e -> Error e


(* Create a directory for the page if necessary.
   If the page is the index page of its section, no directory is necessary.
   Otherwise, "site/foo.html" becomes "build/foo/index.html" to provide
   a clean URL.
 *)
let make_page_dir settings target_dir page_name =
  if page_name = settings.index_page then Ok target_dir
  else
    let target_dir = target_dir +/ page_name in
    try
      FU.mkdir ~parent:true target_dir; Ok target_dir
    with FileUtil.MkdirError e -> Error e

let load_html file =
  try Ok (Soup.read_file file |> Soup.parse)
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

(* Feels wrong to mix the two, this probably should be split into separate functions
   We need a way to exit early if a template is unusable (that is, has no elements
   matching desired selector).
 *)
let get_template file selector =
  try
    let template = Soup.read_file file in
    let html = Soup.parse template in
    let element = Soup.select_one selector html in
    begin
      match element with
      | Some _ -> Ok template
      | None -> Error (Printf.sprintf "Template %s has no element matching selector \"%s\"" file selector)
    end
  with Sys_error e -> Error e

let include_content selector html page_file =
  let content = load_html page_file in
  match content with
  | Ok c ->
    let element = Soup.select_one selector html in
    begin
      match element with
      | Some element -> Ok (Soup.append_child element c)
      | None -> Error (Printf.sprintf "Failed to insert page content: no element matches selector \"%s\"" selector)
    end
  | Error _ as e -> e

(* Widget processing *)
let rec process_widgets settings env ws config soup =
  match ws with
  | [] -> Ok ()
  | w :: ws' ->
    begin
      let open Widgets in
      if not (widget_should_run w.config settings.site_dir env.page_file)
      then (process_widgets settings env ws' config soup) else
      let () = Logs.info @@ fun m -> m "Processing widget %s on page %s" w.name env.page_file in
      let res = w.func env w.config soup in
      (* In non-strict mode, widget processing errors are tolerated *)
      match res, settings.strict with
      | Ok _, _ -> process_widgets settings env ws' config soup
      | Error _ as err, true -> err
      | Error msg, false ->
        let () = Logs.warn @@ fun m -> m "Processing widget \"%s\" failed: %s" w.name msg in
        process_widgets settings env ws' config soup
    end

let process_page env widgets config settings target_dir =
  let page_name = FP.basename env.page_file |> FP.chop_extension in
  let%m target_dir = make_page_dir settings target_dir page_name in
  let%m target_file = Ok (target_dir +/ settings.index_file) in
  let () = Logs.info @@ fun m -> m "Processing page %s" env.page_file in
  let html = Soup.parse env.template in
  let%m () = include_content settings.content_selector html env.page_file in
  let%m () = process_widgets settings env widgets config html in
  let%m () = save_html settings html target_file in
  Ok env.page_file

(* Monad escape... for now *)
let _process_page env widgets config settings target_dir page_file =
    (* Make the page file name accessible to widgets *)
    let env = {env with page_file=page_file} in
    let res = process_page env widgets config settings target_dir in
    match res with
      Ok _ -> ()
    | Error e -> Logs.warn @@ fun m -> m "Error processing page %s: %s" page_file e

(* Process the source directory recursively
   
 *)
let rec process_dir env widgets config settings base_src_dir base_dst_dir dirname =
  let src_path = base_src_dir +/ dirname in
  let dst_path = base_dst_dir +/ dirname in
  let () = Logs.info @@ fun m -> m "Entering directory %s" src_path in
  let nav_path = if dirname <> "" then dirname :: env.nav_path else env.nav_path in
  let env = {env with nav_path = nav_path} in
  let pages = list_page_files src_path in
  let dirs = List.map (FP.basename) (list_dirs src_path) in
  List.iter (_process_page env widgets config settings dst_path) pages;
  ignore @@ List.iter (process_dir env widgets config settings src_path dst_path) dirs

let initialize () =
  let settings = Defaults.default_settings in
  let%m config = Config.read_config Defaults.config_file in
  let settings = Config.update_settings settings config in
  let%m widgets = Widgets.load_widgets config in
  let template_file = Config.get_string_default settings.default_template "default_template" config in
  let content_selector = Config.get_string_default settings.content_selector "content_selector" config in
  let%m default_template = get_template template_file content_selector in
  let default_env = {template=default_template; nav_path=[]; page_file=""} in
  Ok (config, widgets, settings, default_env)
  
let main () =
  let%m config, widgets, settings, default_env = initialize () in
  let () = setup_logging settings.verbose in
  let%m () = make_build_dir settings.build_dir in
  let%m () = Ok (process_dir default_env widgets config settings settings.site_dir settings.build_dir "") in
  return ()

let () =
  let res = main () in
  match res with
  | Ok _ -> exit 0
  | Error e -> Printf.printf "Error: %s\n" e; exit 1
