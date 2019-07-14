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

let check_template filename template selector =
  let soup = Soup.parse template in
  let content_container = Soup.select_one selector soup in
  match content_container with
  | None -> Error (Printf.sprintf "Template %s has no element matching selector \"%s\"" filename selector)
  | Some _ -> Ok ()

let get_template file =
  try Ok (Soup.read_file file)
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
let rec process_widgets settings env ws wh config soup =
  match ws with
  | [] -> Ok ()
  | w :: ws' ->
    begin
      let open Widgets in
      let widget = Hashtbl.find wh w in
      if not (widget_should_run widget.config settings.site_dir env.page_file)
      then (process_widgets settings env ws' wh config soup) else
      let () = Logs.info @@ fun m -> m "Processing widget %s on page %s" w env.page_file in
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
  if page_name = settings.index_page then Utils.safe_tl path
  else path

(*
let run_index_processor settings processor index =
  let json = Autoindex.json_of_entries settings index in
  match processor with
  | None -> Ok ""
  | Some p -> Utils.get_program_output ~input:(Some json) p [| |]
*)

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
  let () = Logs.info @@ fun m -> m "Nav path: %s" (String.concat " " nav_path) in
  let () = Logs.info @@ fun m -> m "Page: %s" page in
  let path = List.append nav_path [page] in
  (* URL path should be absolute *)
  String.concat "/" path |> Printf.sprintf "/%s"

let process_page env index widgets config settings target_dir =
  let page_name = FP.basename env.page_file |> FP.chop_extension in
  (* If clean URLs are used, make_page_dir creates one,
     if not, just returns the current dir *)
  let%m target_dir = make_page_dir settings target_dir page_name in
  let%m target_file =
    if settings.clean_urls then Ok (target_dir +/ settings.index_file)
    (* If clean URLs aren't used, keep the original extension *)
    else Ok (target_dir +/ (FP.basename env.page_file))
  in
  let env = {env with nav_path = (fix_nav_path settings env.nav_path page_name)} in
  let () = Logs.info @@ fun m -> m "Processing page %s" env.page_file in
  let html = Soup.parse env.template in
  let%m () = include_content settings.content_selector html env.page_file in
  let widgets, widget_hash = widgets in
  let%m () = process_widgets settings env widgets widget_hash config html in
  (* Section index injection *)
  let%m () =
    if not settings.index then Ok () else
    let url = make_page_url settings env.nav_path env.page_file in
    if page_name <> settings.index_page
    then let () = index := (Autoindex.get_entry settings url env.nav_path html) :: !index in Ok ()
    else insert_index settings html !index
  in
  let%m () = save_html settings html target_file in
  Ok env.page_file

(* Monad escape... for now *)
let _process_page env index widgets config settings target_dir page_file =
    (* Make the page file name accessible to widgets *)
    let env = {env with page_file=page_file} in
    let res = process_page env index widgets config settings target_dir in
    match res with
      Ok _ -> ()
    | Error e -> Logs.warn @@ fun m -> m "Error processing page %s: %s" page_file e

(* Reorders the pages so that the index page is processed last,
   when the section index is available *)
let reorder_pages settings ps =
  let find_index = fun p -> (FP.basename p |> FP.chop_extension) = settings.index_page in
  let index_page = List.find find_index ps in
  let ps = CCList.remove ~eq:(=) ~key:index_page ps in
  List.append ps [index_page]

(* Process the source directory recursively
   
 *)
let rec process_dir env widgets config settings base_src_dir base_dst_dir dirname =
  let index = ref [] in
  let src_path = base_src_dir +/ dirname in
  let dst_path = base_dst_dir +/ dirname in
  let () = Logs.info @@ fun m -> m "Entering directory %s" src_path in
(*  let nav_path = if dirname <> "" then dirname :: env.nav_path else env.nav_path in *)
  let nav_path = if dirname <> "" then List.append env.nav_path [dirname] else env.nav_path in
  let env = {env with nav_path = nav_path} in
  let pages = list_page_files src_path |> reorder_pages settings in
  let dirs = List.map (FP.basename) (list_dirs src_path) in
  List.iter (_process_page env index widgets config settings dst_path) pages;
  ignore @@ List.iter (process_dir env widgets config settings src_path dst_path) dirs

(* Option parsing and initialization *)

let get_args settings =
  let init = ref false in
  let strict = ref settings.strict in
  let verbose = ref settings.verbose in
  let args = [
    ("--init", Arg.Unit (fun () -> init := true), "Setup basic directory structure");
    ("--verbose", Arg.Unit (fun () -> verbose := true), "Verbose output");
    ("--strict", Arg.Bool (fun s -> strict := s), "<true|false> Stop on page processing errors or not");
    ("--version", Arg.Unit (fun () -> Utils.print_version (); exit 0), "Print version and exit")
  ]
  in let usage = Printf.sprintf "Usage: %s [OPTIONS]" Sys.argv.(0) in
  let () = Arg.parse args (fun _ -> ()) usage in
  let settings = {settings with verbose = !verbose; strict = !strict} in
  if !init then (Project_init.init settings; exit 0) else Ok settings

let initialize () =
  let settings = Defaults.default_settings in
  let () = setup_logging settings.verbose in
  let%m config = Config.read_config Defaults.config_file in
  let settings = Config.update_settings settings config in
  let%m settings = get_args settings in
  let%m widgets = Widgets.get_widgets config in
  let%m default_template_str = get_template settings.default_template in
  let%m () = check_template settings.default_template default_template_str settings.content_selector in
  let default_env = {template=default_template_str; nav_path=[]; page_file=""} in
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
