open Defaults

module FU = FileUtil
module FP = FilePath

let (+/) left right =
    FP.concat left right

(* Yet another error monad *)
let bind r f =
  match r with
    Ok r -> f r
  | Error _ as err -> err

let return x = Ok x

(* Logging setup *)


let setup_logging verbose =
  let level = if verbose then Logs.Info else Logs.Warning in
  Logs.set_level (Some level);
  Logs.set_reporter (Logs_fmt.reporter ())

(* Filesystem stuff *)
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

let save_html soup file =
  try Ok (Soup.pretty_print soup |> Soup.write_file file)
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

let process_page settings env target_dir page_file =
  let page_name = FP.basename page_file |> FP.chop_extension in
  let%m target_dir = make_page_dir settings target_dir page_name in
  let%m target_file = Ok (target_dir +/ settings.index_file) in
  let () = Logs.info @@ fun m -> m "Processing page %s" page_file in
  let html = Soup.parse env.template in
  let%m () = include_content settings.content_selector html page_file in
  let%m () = save_html html target_file in
  Ok page_file

(* Monad escape... for now *)
let _process_page settings env target_dir page_file =
    let res = process_page settings env target_dir page_file in
    match res with
      Ok _ -> ()
    | Error e -> Logs.warn @@ fun m -> m "Error processing page %s: %s" page_file e

(* Process the source directory recursively
   
 *)
let rec process_dir settings env base_src_dir base_dst_dir dirname =
    let src_path = base_src_dir +/ dirname in
    let dst_path = base_dst_dir +/ dirname in
    let () = Logs.info @@ fun m -> m "Entering directory %s" src_path in
    let nav_path = if dirname <> "" then dirname :: env.nav_path else env.nav_path in
    let env = {env with nav_path = nav_path} in
    let pages = list_page_files src_path in
    let dirs = List.map (FP.basename) (list_dirs src_path) in
    let () = List.iter (_process_page settings env dst_path) pages;
             ignore @@ List.iter (process_dir settings env src_path dst_path) dirs
    in ()

let initialize () =
  let settings = Defaults.default_settings in
  let%m config = Config.read_config Defaults.config_file in
  let settings = Config.update_settings settings config in
  let%m default_template = get_template settings.default_template settings.content_selector in
  let default_env = {template=default_template; nav_path=[]} in
  Ok (config, settings, default_env)
  
let main () =
  let%m _, settings, default_env = initialize () in
  let () = setup_logging settings.verbose in
  let%m () = make_build_dir settings.build_dir in
  let%m () = Ok (process_dir settings default_env settings.site_dir settings.build_dir "") in
  return ()

let () =
  let res = main () in
  match res with
  | Ok _ -> exit 0
  | Error e -> Printf.printf "Error: %s\n" e; exit 1
