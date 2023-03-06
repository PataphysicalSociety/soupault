open Soupault_common
open Defaults

module FU = FileUtil
module FP = FilePath

(* Result monad *)
let (>>=) = Stdlib.Result.bind
let (let*) = (>>=)

let mkdir dir =
  (* Note: FileUtil.mkdir returns success if the directory
     already exists, this is why it's not checked before creation. *)
  try Ok (FU.mkdir ~parent:true dir)
  with FileUtil.MkdirError e -> Error e

(*** Logging setup ***)

(* Determines whether log coloring is appropriate in the environment where soupault is running.

   As of 4.2.0, soupault officially supports two kinds of OSes: UNIX-like and Windows.
   On UNIX-like systems, most terminals support ANSI colors,
   so we enable color by default but give the user an option to disable it
   by using the NO_COLOR environment variable (see https://no-color.org).

   We also disable coloring if soupault is sending its output to a pipe rather than a terminal,
   as any well-behaved program should.

   On Windows, most terminals still don't support colors,
   so we always disable colors on that platform.

   If a portable way to test terminal color capabilities is ever developed,
   of course it would be nice to make this check more granular.
 *)
let get_color_style () =
  if Sys.win32 then `None else
  let no_color = Sys.getenv_opt "NO_COLOR" |> Option.is_some in
  (* Logs always go to stderr, so we don't check if stdout is a TTY. *)
  let interactive = Unix.isatty (Unix.descr_of_out_channel stderr) in
  if interactive && (not no_color) then `Ansi_tty else `None 

let color_level ppf l =
  let app_style = `Cyan in
  let err_style = `Red in
  let warn_style = `Yellow in
  let info_style = `Green in
  let debug_style = `Blue in
  let f ppf style l =
    Fmt.pf ppf "%a" Fmt.(styled style string)
      (Logs.level_to_string (Some l) |> String.uppercase_ascii)
  in
  let l = if Option.is_none l then Logs.App else Option.get l in
  match l with
  | Logs.App ->
    f ppf app_style l
  | Logs.Error ->
    f ppf err_style l
  | Logs.Warning ->
    f ppf warn_style l
  | Logs.Info ->
    f ppf info_style l
  | Logs.Debug ->
    f ppf debug_style l

(* Sets the log format to "[$level] $msg".

   The default log format of Daniel Bünzli's Logs includes executable name in the message,
   which is useful in some situations, but I doubt it's helpful for typical soupault use.
 *)
let pp_header ppf (l, h) =
  match h with
  | None -> if l = Logs.App then () else Format.fprintf ppf "[%a] " color_level (Some l)
  | Some h -> Format.fprintf ppf "[%s] " h

(* Performs logging setup.

   The traditional way to handle verboseness is [-v], [-vv], [-vvv]...
   I believe separate [--verbose] and [--debug] flags are more intuitive
   and that UI improvement justifies somewhat awkward implementation
   of the log level check.
 *)
let setup_logging verbose debug =
  let level =
    if debug then Logs.Debug
    else if verbose then Logs.Info
    else Logs.Warning
  in
  let style = get_color_style () in
  Logs.set_level (Some level);
  Fmt_tty.setup_std_outputs ~style_renderer:style ();
  Logs.set_reporter @@ Logs.format_reporter ~pp_header:pp_header ();
  (* Enable exception tracing if debug=true *)
  if debug then Printexc.record_backtrace true

(*** Filesystem stuff ***)

let (+/) left right =
    FP.concat left right

let list_dirs path =
    FU.ls path |> FU.filter FU.Is_dir

let make_build_dir build_dir =
  if (FU.test FU.Exists build_dir) then Ok () else
  let () = Logs.info @@ fun m -> m {|Build directory "%s" does not exist, creating|} build_dir in
  mkdir build_dir

(*** Page processing helpers. ***)

(* Produces a target directory name for the page.

   If clean URLs are used, then a subdirectory matching the page name
   should be created inside the section directory, unless the page is
   a section index page.
   E.g. "site/foo.html" becomes "build/foo/index.html" to provide
   a clean URL.

   If clean URLs are not used, only section dirs are created.
 *)
let make_page_dir_name settings target_dir page_name =
  if (page_name = settings.index_page) || (not settings.clean_urls) then target_dir
  else target_dir +/ page_name

(* Finds a preprocessor for specific file name,
   if a preprocessor for its extension is configured.

   If a file has multiple extensions, soupault only consideres the last one.

   This function is used for both page preprocessors (in the "[preprocessors]" config section)
   and asset processors ("[asset_processors]").
 *)
let find_preprocessor preprocessors file_name =
  try
    let ext = File_path.get_extension file_name in
    List.assoc_opt ext preprocessors
  with Utils.Malformed_file_name _ ->
    (* Since page file names come from directory listings,
       they are guaranteed to be valid, non-reserved names.
       But if this "can't happen" situation does happen,
       it's probably better to have a distinctive error for it.
     *)
    internal_error "Nomen tuum malum est, te pudeat!"

let load_html settings soupault_config hooks page_file =
  let load_file page_preprocessor page_file =
    try
    match page_preprocessor with
    | None ->
      let page_source = Soup.read_file page_file in
      let () = Cache.refresh_page_cache settings page_file page_source in
      Ok page_source
    | Some prep ->
      if settings.caching then
        begin
          (* Check if we have converted HTML source cached.

             Since preprocessors get page file path as an argument rather than stdin
             (to accomodate preprocessors that do not support reading from stdin),
             soupault does not read page files unless it knows they are not subject to preprocessing.

             However, to check if we have a cached object, we need the source string for it.
             So, if caching is enabled, we always read the page first, to check if we have a cached version.
             This causes useless file reads when it's a cache miss and we need to run a preprocessor,
             but reading files is still much faster than executing external programs.
           *)
          let page_source = Soup.read_file page_file in
          let () = Cache.refresh_page_cache settings page_file page_source in
          let cached = Cache.get_cached_object settings page_file prep page_source in
          if Option.is_some cached then Ok (Option.get cached) else
          (* If not, run the preprocessor to get the HTML source. *)
          let prep_cmd = Printf.sprintf "%s %s" prep (Filename.quote page_file) in
          let () = Logs.info @@ fun m -> m {|Calling page preprocessor "%s" on page %s|} (String.escaped prep) page_file in
          let output = Process_utils.get_program_output prep_cmd in
          match output with
          | Ok output ->
            (* Cache the object for future use. *)
            let () = Cache.cache_object settings page_file prep page_source output in
            Ok output
          | (Error _) as e -> e
        end
      else
        begin
          let prep_cmd = Printf.sprintf "%s %s" prep (Filename.quote page_file) in
          let () = Logs.info @@ fun m -> m {|Calling page preprocessor "%s" on page %s|} (String.escaped prep) page_file in
          Process_utils.get_program_output prep_cmd
        end
    with Sys_error e -> Error e
  in
  let page_preprocessor = find_preprocessor settings.page_preprocessors page_file in
  let* page_source = load_file page_preprocessor page_file in
  let pre_parse_hook = Hashtbl.find_opt hooks "pre-parse" in
  match pre_parse_hook with
  | Some (file_name, source_code, hook_config) ->
    if Hooks.hook_should_run settings hook_config "pre-parse" page_file then
      let () = Logs.info @@ fun m -> m {|Running the "pre-parse" hook on page %s|} page_file in
      Hooks.run_pre_parse_hook settings soupault_config hook_config file_name source_code page_file page_source
    else Ok page_source
  | None -> Ok page_source

let parse_html page_source =
  (* As of lambdasoup 0.7.2, Soup.parse never fails, only returns empty element trees,
     so there's no need to handle errors here.
   *)
  Ok (Soup.parse page_source)

(* The built-in HTML rendering function that is used when the "render" hook is not configured. *)
let render_html_builtin settings soup =
  let add_doctype settings html_str =
    (* 32K is probably a sensible guesstimate of the average page size *)
    let buf = Buffer.create (32 * 1024) in
    let doctype = settings.doctype |> String.trim in
    let () =
      Buffer.add_string buf doctype;
      Buffer.add_char buf '\n';
      Buffer.add_string buf html_str
    in
    Buffer.contents buf
  in
  let print_html = if settings.pretty_print_html then Soup.pretty_print else Soup.to_string in
  if settings.keep_doctype then
    begin
      let html_str = print_html soup in
      (* If we are given an empty page, then adding doctype makes no sense, we just return an empty string. *)
      if String.length html_str = 0 then "" else
      let has_doctype =
        (<>) 0 (Re.matches (Re.Perl.compile_pat ~opts:[`Caseless] "^(<!DOCTYPE[^>]*>)") html_str |> List.length)
      in
      (* Can the page be an invalid, incomplete HTML? Of course it can,
         but if the user chose to force a doctype, it's their responsibilty.
       *)
      if not has_doctype then
        add_doctype settings html_str
      else html_str
    end
  else
    begin
      (* If we are to discard the original doctype and completely replace it,
         we need to remove the original one.

         XXX: As of lambdasoup 0.7.2, there's no way to delete the doctype "element"
         (which isn't actually an element anyway),
         so we extract the <html> from the document tree,
         and prepend a doctype to it.
         That is, if the document even has <html> to begin with--see below. *)
      let html = Soup.select_one "html" soup in
      match html with
      | Some html ->
        let html_str = print_html html in
        add_doctype settings html_str
      | None ->
        (* This may happen if a page (in postprocessor mode)
           or a page template (in generator mode) is incomplete.
           At the moment soupault doesn't prohibit invalid HTML,
           so we need to handle this case.
         *)
        let () = Logs.warn @@ fun m -> m "Page has no <HTML> element, not setting doctype" in
        print_html soup
    end

(* The high-level render call that will either run the "render" hook or use the built-in renderer
   if the hook is not configured or page is excluded from it.
 *)
let render_html settings config hooks env soup =
  let hook = Hashtbl.find_opt hooks "render" in
  match hook with
  | Some (file_name, source_code, hook_config) ->
    if not (Hooks.hook_should_run settings hook_config "render" env.page_file)
    then Ok (render_html_builtin settings soup)
    else
      let () = Logs.info @@ fun m -> m {|Running the "render" hook on page %s|} env.page_file in
      let* page_source = Hooks.run_render_hook
        settings config hook_config file_name source_code env soup
      in Ok page_source
  | None -> Ok (render_html_builtin settings soup)

(* Injects page content into a template.
   Where exactly it will insert it is defined by the [content_selector] option in the template config,
   or by [settings.default_content_selector] if the default template is used.
 *)
let include_content action selector html content =
  let element = Soup.select_one selector html in
  match element with
  | Some element -> Ok (Html_utils.insert_element (Some action) element content)
  | None ->
    Error (Printf.sprintf {|No element in the template matches selector "%s", nowhere to insert the content|}
           selector)

(* Produces a complete page.
   In generator mode and for partial pages (those that aren't complete HTML documents)
   that means insert page content loaded from file into a template.
   Pages that are already complete documents are returned unchanged.
 *)
let make_page settings page_file_path content =
  (* If generator mode is off, treat everything like a complete page *)
  if not settings.generator_mode then Ok content else
  let page_wrapper_elem = Soup.select_one settings.complete_page_selector content in
  (* If page file appears to be a complete page rather than a page body,
     just return it *)
  match page_wrapper_elem with
  | Some _ ->
    let () =
      if settings.generator_mode then
      Logs.debug @@ fun m -> m "File appears to be a complete page, not using the page template"
      (* in HTML processor mode that's implied *)
    in Ok content
  | None ->
    let tmpl = List.find_opt
      (fun t -> (Path_options.page_included settings t.template_path_options settings.site_dir page_file_path) = true)
      settings.page_templates
    in
    let html, content_selector, content_action = (match tmpl with
      | None ->
        let () = Logs.info @@ fun m -> m "Using the default template for page %s" page_file_path in
        (Soup.parse settings.default_template_source,
         Some settings.default_content_selector,
         Some settings.default_content_action)
      | Some t ->
        let () = Logs.info @@ fun m -> m {|Using template "%s" for page %s|} t.template_name page_file_path in
        (Soup.parse t.template_data,
         t.template_content_selector,
         t.template_content_action))
    in
    let content_selector = Option.value ~default:settings.default_content_selector content_selector in
    let content_action = Option.value ~default:settings.default_content_action content_action in
    let* () = include_content content_action content_selector html content in
    Ok html

(* Widget processing *)
let rec process_widgets env settings ws wh config soup =
  match ws with
  | [] -> Ok ()
  | w :: ws' ->
    begin
      let open Widgets in
      let widget = Hashtbl.find wh w in
      if not (widget_should_run settings w widget env.page_file)
      then (process_widgets env settings ws' wh config soup) else
      let () = Logs.info @@ fun m -> m "Processing widget %s on page %s" w env.page_file in
      let res =
        try widget.func env widget.config soup
        with 
        | Soupault_error s -> Error s
        | Config.Config_error s -> Error s
      in
      (* In non-strict mode, widget processing errors are tolerated *)
      match res, settings.strict with
      | Ok _, _ -> process_widgets env settings ws' wh config soup
      | Error _ as err, true -> err
      | Error msg, false ->
        let () = Logs.warn @@ fun m -> m {|Processing widget "%s" failed: %s|} w msg in
        process_widgets env settings ws' wh config soup
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

(** Decide on the page file name.

    If clean URLs are used, it's always <target_dir>/<settings.index_file>

    If clean URLs are not used, then the base file name is preserved.
    The extension, however, is set to settings.default_extension,
    unless it's in the settings.keep_extensions list.

    The reason for this extension juggling is that people may use page preprocessors
    but not use clean URLs, without extension mangling they will end up
    with pages like build/about.md that have HTML inside despit their name.
    In short, that's what Jekyll et al. always did to non-blog pages.
 *)
let make_page_file_name settings page_file target_dir =
  if settings.clean_urls then (target_dir +/ settings.index_file) else
  let page_file = FP.basename page_file in
  let extension = File_path.get_extension page_file in
  let page_file =
    if Utils.in_list extension settings.keep_extensions then page_file
    else FP.add_extension (FP.chop_extension page_file) settings.default_extension
  in target_dir +/ page_file

let save_html settings soupault_config hooks env page_source =
  let save_hook = Hashtbl.find_opt hooks "save" in
  match save_hook with
  | Some (file_name, source_code, hook_config) ->
    if Hooks.hook_should_run settings hook_config "save" env.page_file then
      Hooks.run_save_hook settings soupault_config hook_config file_name source_code env page_source
    else Utils.write_file env.target_file page_source
  | None ->
    let () = Logs.info @@ fun m -> m "Writing generated page to %s" env.target_file in
    Utils.write_file env.target_file page_source

let run_post_save_hook settings soupault_config hooks env =
  let post_save_hook = Hashtbl.find_opt hooks "post-save" in
  match post_save_hook with
  | Some (file_name, source_code, hook_config) ->
    if Hooks.hook_should_run settings hook_config "post-save" env.page_file then
      Hooks.run_post_save_hook settings soupault_config hook_config file_name source_code env
    else Ok ()
  | None ->
    Ok ()

let make_page_url settings nav_path orig_path target_dir page_file =
  let orig_page_file_name = FP.basename page_file in
  let target_page =
    if settings.clean_urls then target_dir |> FP.basename
    else make_page_file_name settings orig_page_file_name ""
  in
  let path =
    if ((FP.chop_extension orig_page_file_name) = settings.index_page) then orig_path
    else (List.append nav_path [target_page])
  in
  (* URL path should be absolute *)
  String.concat "/" path |> Printf.sprintf "/%s"

let extract_metadata settings soupault_config hooks env html =
  (* Metadata is only extracted from non-index pages *)
  if not (Autoindex.index_extraction_should_run settings env.page_file) then (Ok None) else
  let entry = Autoindex.get_entry settings env html in
  let post_index_hook = Hashtbl.find_opt hooks "post-index" in
  match post_index_hook with
  | Some (file_name, source_code, hook_config) ->
    if not (Hooks.hook_should_run settings hook_config "post-index" env.page_file) then (Ok (Some entry)) else
    (* Let the post-index hook update the fields *)
    let* index_fields =
      let () = Logs.info @@ fun m -> m {|Running the "post-index" hook on page %s|} env.page_file in
      Hooks.run_post_index_hook settings soupault_config hook_config file_name source_code env html entry.fields
    in
    Ok (Some {entry with fields=index_fields})
  | None -> Ok (Some entry)

let run_pre_process_hook settings config hooks page_file target_dir target_file content =
  let pre_process_hook = Hashtbl.find_opt hooks "pre-process" in
  match pre_process_hook with
  | Some (file_name, source_code, hook_config) ->
    if not (Hooks.hook_should_run settings hook_config "pre-process" page_file)
    then Ok (target_dir, target_file, content)
    else
      let () = Logs.info @@ fun m -> m {|Running the "pre-process" hook on page %s|} page_file in
      Hooks.run_pre_process_hook
        settings config hook_config file_name source_code page_file target_dir target_file content
  | None -> Ok (target_dir, target_file, content)

(* Check if index insertion should be done and log the reason if not *)
let index_insertion_should_run settings index page_name =
  let aux settings page_name =
    if not settings.index then Some "indexing is disabled in the configuration" else
    if settings.index_only then Some "running in the index-only mode" else
    if (page_name <> settings.index_page) then Some (Printf.sprintf "page name does not match %s" settings.index_page) else
    if index = [] then Some "index is empty" else None
  in
  match (aux settings page_name) with
  | None -> true
  | Some msg ->
    let () = Logs.debug @@ fun m -> m "Not inserting index data: %s" msg in
    false

(** Processes a page:

    1. Adjusts the path to account for index vs non-index page difference
       in setups using clean URLs
    2. Reads a page file and inserts the content into the template,
       unless it's a complete page
    3. Updates the global index if necessary
    4. Runs the page through widgets
    5. Inserts the index section into the page if it's an index page
    6. Saves the processed page to file
  *)
let process_page page_data index index_hash widgets hooks config settings =
  let (page_file, page_content, nav_path) = (page_data.page_file_path, page_data.page_content, page_data.page_nav_path) in
  let () = Logs.info @@ fun m -> m "Processing page %s" page_file in
  let* page_source =
    match page_content with
    | None ->
      (* This is a real page that actually exists on disk. *)
      load_html settings config hooks page_file
    | Some content ->
      (* This is a "fake" paginated index or taxonomy page created by an index processor. *)
      let () = Cache.refresh_page_cache settings page_file content in
      Ok content
  in
  let* content = parse_html page_source in
  let page_name = FP.basename page_file |> FP.chop_extension in
  let orig_path = nav_path in
  let nav_path = fix_nav_path settings nav_path page_name in
  let target_dir = make_page_dir_name settings (File_path.concat_path orig_path) page_name |> FP.concat settings.build_dir in
  let target_file = make_page_file_name settings page_file target_dir in
  let* (target_dir, target_file, content) =
    run_pre_process_hook settings config hooks page_file target_dir target_file content
  in
  let page_url = make_page_url settings nav_path orig_path target_dir page_file in
  let env = {
    nav_path = nav_path;
    page_url = page_url;
    page_file = page_file;
    target_dir = target_dir;
    target_file = target_file;
    site_index = index;
    site_index_hash = index_hash;
    settings = settings;
  }
  in
  let* html = make_page settings page_file content in
  (* Section index injection always happens before any widgets have run *)
  let* new_pages =
    (* Section index is inserted only in index pages *)
    if not (index_insertion_should_run settings index page_name) then Ok []
    else let () = Logs.info @@ fun m -> m "Inserting section index into page %s" page_file in
    Autoindex.insert_indices env config html
  in
  let before_index, after_index, widget_hash = widgets in
  let* () = process_widgets env settings before_index widget_hash config html in
  (* Index extraction *)
  let* index_entry = extract_metadata settings config hooks env html in
  if settings.index_only then Ok (index_entry, new_pages) else
  let* () = process_widgets env settings after_index widget_hash config html in
  let* () = mkdir target_dir in
  let* html_str = render_html settings config hooks env html in
  let* () = save_html settings config hooks env html_str in
  (* Finally, run the post-save hook. *)
  let* () = run_post_save_hook settings config hooks env in
  Ok (index_entry, new_pages)

(* Monadic wrapper for process_page that can either return or ignore errors  *)
let process_page index index_hash widgets hooks config settings page_data =
  let res =
    try process_page page_data index index_hash widgets hooks config settings
    with Soupault_error msg -> Error msg
  in
  match res with
    Ok _ as res -> res
  | Error msg ->
    let msg = Printf.sprintf "Could not process page %s: %s" page_data.page_file_path msg in
    if settings.strict then Error msg else 
    let () = Logs.warn @@ fun m -> m "%s" msg in
    Ok (None, [])

(* Option parsing and initialization *)

type soupault_action =
  | BuildWebsite
  | InitProject
  | ShowVersion
  | ShowVersionNumber
  | ShowDefaultConfig
  | ShowEffectiveConfig

(* Intermediate data structure for CLI options
   that we will later use to inject CLI overrides into the config loaded from a file.
 *)
type cli_options = {
  action: soupault_action;
  config_file_opt: string option;
  build_profiles_opt: string list;
  verbose_opt: bool option;
  debug_opt: bool option;
  strict_opt: bool option;
  site_dir_opt: string option;
  build_dir_opt: string option;
  index_only_opt: bool option;
  dump_index_json_opt: string option;
  force_opt: bool option;
  caching_opt : bool option;
}

let default_cli_options = {
  (* Assume the user wants to build a website unless specified otherwise. *)
  action = BuildWebsite;
  (* Everything else is assumed to be determined by the config unless
     overridden by a CLI option.
   *)
  config_file_opt = None;
  verbose_opt = None;
  debug_opt = None;
  strict_opt = None;
  site_dir_opt = None;
  build_dir_opt = None;
  build_profiles_opt = [];
  index_only_opt = None;
  dump_index_json_opt = None;
  force_opt = None;
  caching_opt = None;
}

let usage_msg = Printf.sprintf {|Usage: %s [OPTIONS]

Soupault is a static site generator based on HTML element tree rewriting.
Visit https://soupault.app/reference-manual/ for documentation.

To build your website, you can simply run %s without any arguments
or specify --build explicitly if you insist.

You can specify a path to a custom config file using --config option
or SOUPAULT_CONFIG environment variable.

Options:
|} Sys.argv.(0) Sys.argv.(0)

let get_args () =
  let actions = ref [] in
  let opts = ref default_cli_options in
  let args = Arg.align [
    (* "Action" flags that make soupault do something else than a website build. *)
    ("--build", Arg.Unit (fun () -> actions := (BuildWebsite :: !actions)), " Build a website (default action)");
    ("--init", Arg.Unit (fun () -> actions := (InitProject :: !actions)), " Set up basic directory structure");
    ("--show-default-config", Arg.Unit (fun () -> actions := (ShowDefaultConfig :: !actions)), " Print the default config and exit");
    ("--show-effective-config", Arg.Unit (fun () -> actions := (ShowEffectiveConfig :: !actions)), " Print the effective config (user-defined and default options) and exit");
    ("--version", Arg.Unit (fun () -> actions := (ShowVersion :: !actions)), " Print version and copyright information and exit");
    ("--version-number", Arg.Unit (fun () -> actions := (ShowVersionNumber :: !actions)), " Print version number and exit");
    (* "Option" flags that change website build behavior. *)
    ("--config", Arg.String (fun s -> opts := {!opts with config_file_opt=(Some s)}), "<PATH>  Configuration file path");
    ("--verbose", Arg.Unit (fun () -> opts := {!opts with verbose_opt=(Some true)}), " Output build progress and informational messages");
    ("--debug", Arg.Unit (fun () -> opts := {!opts with debug_opt=(Some true)}), " Output debug information");
    ("--strict", Arg.Bool (fun s -> opts := {!opts with strict_opt=(Some s)}), "<true|false>  Stop on page processing errors or not");
    ("--site-dir", Arg.String (fun s -> opts := {!opts with site_dir_opt=(Some s)}), "<DIR>  Directory with input files");
    ("--build-dir", Arg.String (fun s -> opts := {!opts with build_dir_opt=(Some s)}), "<DIR>  Output directory");
    ("--profile", Arg.String (fun s -> opts := {!opts with build_profiles_opt=(s :: !opts.build_profiles_opt)}), "<NAME>  Build profile (you can give this option more than once)");
    ("--index-only", Arg.Unit (fun () -> opts := {!opts with index_only_opt=(Some true)}), " Extract site index without generating pages");
    ("--dump-index-json", Arg.String (fun s -> opts := {!opts with dump_index_json_opt=(Some s)}), "<PATH>  Dump extracted index into a JSON file");
    ("--force", Arg.Unit (fun () -> opts := {!opts with force_opt=(Some true)}), " Force generating all target files");
    ("--no-caching", Arg.Unit (fun () -> opts := {!opts with caching_opt=(Some false)}), " Disable caching (overrides settings.caching)");
  ]
  in
  let () = Arg.parse args (fun _ -> ()) usage_msg in
  match !actions with
  | [] ->
    (* Nothing is specified, that means we perform the default action (build). *)
    !opts
  | [a] ->
    (* One non-default action option is specified, that's normal. *)
    {!opts with action=a}
  | _ ->
    (* This function is first called at a point when the logger isn't setup yet,
       so we only have standard printing functions to tell the user about errors. *)
    let () =
      print_endline "Error: Incorrect comand line option combination.";
      print_endline "Please specify only one of --init, --version, --help, --show-default-config, or --show-effective-config";
      print_endline "To build your website, simply run soupault without any options."
    in
    exit 1

(* Soupault allows overriding config settings with CLI options.
   For example, running [soupault --debug] will enable debug logging even if [settings.debug] is [false].

   This function injects those overrides if they are defined.
 *)
let update_settings settings cli_options =
  let sr = ref settings in
  let () =
    if Option.is_some cli_options.debug_opt then sr := {!sr with debug=(Option.get cli_options.debug_opt)};
    if Option.is_some cli_options.verbose_opt then sr := {!sr with verbose=(Option.get cli_options.verbose_opt)};
    if Option.is_some cli_options.strict_opt then sr := {!sr with strict=(Option.get cli_options.strict_opt)};
    if Option.is_some cli_options.force_opt then sr := {!sr with force=(Option.get cli_options.force_opt)};
    if Option.is_some cli_options.site_dir_opt then sr := {!sr with site_dir=(Option.get cli_options.site_dir_opt)};
    if Option.is_some cli_options.build_dir_opt then sr := {!sr with build_dir=(Option.get cli_options.build_dir_opt)};
    if Option.is_some cli_options.index_only_opt then sr := {!sr with index_only=(Option.get cli_options.index_only_opt)};
    if Option.is_some cli_options.dump_index_json_opt then sr := {!sr with dump_index_json=cli_options.dump_index_json_opt};
    if Option.is_some cli_options.caching_opt then sr := {!sr with caching=(Option.get cli_options.caching_opt)};
    sr := {!sr with build_profiles=cli_options.build_profiles_opt}
  in !sr

let check_project_dir settings =
  let () =
    if (not (FU.test FU.Exists settings.default_template)) && settings.generator_mode then
    (* Don't make this fatal just yet, because:
         a) either it will blow up very soon after anyway, when soupault gets to the first page
         b) or ther user specified a custom template for every path.
     *)
    Logs.warn @@ fun m -> m {|Default template is required in generator mode, but template file "%s" does not exist|}
      settings.default_template
  in let () =
    if (not (FU.test FU.Is_dir settings.site_dir))
    then begin
      (* If there's no site_dir under the current working directory,
         then either the user accidentally ran soupault in a completely wrong directory
         or their project isn't initialized yet.
       *)
      Logs.err @@ fun m -> m {|Site directory "%s" does not exist!|} settings.site_dir;
      Logs.err @@ fun m -> m "You can use %s --init to initialize a basic project" Sys.argv.(0);
      exit 1
    end
  in ()

(* As of soupault 4.2.0, there are two possible default config file names.

   The original config file name was soupault.conf,
   but in 2.0.0 the default was changed to soupault.toml
   to make it clear what the config format is.
   For backward compatibility, both variants are still supported.
 *)
let find_default_config_file () =
  let conf_exists = Sys.file_exists Defaults.config_file in
  let alt_conf_exists = Sys.file_exists Defaults.config_file_alt in
  match conf_exists, alt_conf_exists with
  | true, false -> Ok Defaults.config_file
  | false, true -> Ok Defaults.config_file_alt
  | true, true ->
    let () = Logs.warn @@ fun m -> m "Both %s and %s files exist, using %s"
      Defaults.config_file Defaults.config_file_alt Defaults.config_file
    in Ok Defaults.config_file
  | false, false ->
      let () =
        Logs.err @@ fun m -> m "Could not find either %s or %s in the current directory."
          Defaults.config_file Defaults.config_file_alt;
        Logs.err @@ fun m -> m "Make sure you are in a soupault project directory or specify configuration file location in \
          %s environment variable." Defaults.config_path_env_var
      in
      Error "Cannot proceed without a configuration file."

(* There are two ways to specify a custom config file path:
   either set the [SOUPAULT_CONFIG] environment variable
   or use --config command line option.

   If both are defined, [--config] takes precedence
   because it's supposed to be a way to override everything:
   both the default config path and the environment variable
   if it's set globally in the system.
 *)
let find_config_file cli_options =
  let config_env_var =
    try Some (Unix.getenv Defaults.config_path_env_var)
    with Not_found -> None
  in
  match config_env_var, cli_options.config_file_opt with
  | Some path, None -> Ok path
  | None, Some path -> Ok path
  | Some _, Some path ->
    let () = Logs.warn @@ fun m -> m "Both SOUPAULT_CONFIG environment variable and --config option are given, using --config" in
    Ok path
  | None, None ->
    find_default_config_file ()

let show_startup_message settings =
  let mode = if settings.generator_mode then "website generator" else "HTML processor" in
  Logs.info @@ fun m -> m "Starting soupault %s in %s mode" Defaults.version_string mode

let initialize cli_options =
  (* Soupault itself doesn't use the PRNG in any way,
     but it exposes a random function in the plugin API,
     so it needs to initialize the RNG to avoid completely predictable sequences.
   *)
  let () = Random.self_init () in
  let settings = Defaults.default_settings in
  let () = setup_logging settings.verbose settings.debug in
  let* config_file = find_config_file cli_options in
  let* config = Config.read_config config_file in
  (* First, populate the settings from the config file data. *)
  let* settings = Config.update_settings settings config in
  (* Then override options in it with values from command line arguments, if there are any. *)
  let settings = update_settings settings cli_options in
  (* Update the log level from the config and arguments  *)
  let () = setup_logging settings.verbose settings.debug in
  let () = check_project_dir settings in
  let* config = Ok (config |> Option.get) in
  (* Inject defaults and updated values back into the TOML config
     to make the complete effective settings available to plugins and visible in --show-effective-config. *)
  let config = Config.inject_options settings config in
  let () = show_startup_message settings in
  let* plugins = Plugins.get_plugins settings (Some config) in
  let* widgets = Widgets.get_widgets settings (Some config) plugins settings.index_extract_after_widgets in
  let* hooks = Hooks.get_hooks config in
  let* default_template_str =
    if settings.generator_mode then Utils.read_file settings.default_template
    else Ok ""
  in
  let settings = {settings with default_template_source=default_template_str} in
  let () =
    begin
      if not settings.generator_mode then
        Logs.info @@ fun m -> m "Running in HTML processor mode, not using page templates";
      if settings.index_only && not (settings.index && (settings.dump_index_json <> None)) then
        Logs.warn @@ fun m -> m "--index-only is useless without index=true and dump_json options in the config!";
    end
  in
  if settings.site_dir = "" then (Error "site_dir must be a directory path, not an empty string")
  else if settings.build_dir = "" then (Error "build_dir must be a directory path, not an empty string")
  else (Ok (config, widgets, hooks, settings))

let dump_index_json settings index =
  match settings.dump_index_json with
  | None -> Ok ()
  | Some f ->
    let () = Logs.info @@ fun m -> m "Exporting index data to JSON file %s" f in
    try Ok (Soup.write_file f @@ Autoindex.json_string_of_entries index)
    with Sys_error e -> Error e

let check_version settings =
  match settings.soupault_version with
  | None -> ()
  | Some v ->
    try
      let res = Version.require_version v in
      if res then () else begin
        Printf.printf "According to settings.soupault_version, this configuration file is for soupault %s\n" v;
        Printf.printf "You are running soupault version %s, older than required\n" Defaults.version_string;
        Printf.printf "To proceed, upgrade soupault to at least %s, or (at your own risk) \
          remove the soupault_version option from your configuration\n" v;
        exit 1
      end
    with Failure msg -> begin
      Printf.printf "Could not check configuration compatibility with running soupault version: %s\n" msg;
      print_endline "Maybe your settings.soupault_version option is malformed?\n";
      exit 1
    end

let process_page_files index_hash widgets hooks config settings files =
  Utils.fold_left_result
    (fun acc p ->
      let ie = process_page [] index_hash widgets hooks config settings p in
       match ie with Ok (None, _) -> Ok acc | Ok (Some ie', _) -> Ok (ie' :: acc) | Error _ as err -> err)
    []
    files

let process_index_files index index_hash widgets hooks config settings files =
  Utils.fold_left_result
    (fun acc p ->
      let ie = process_page index index_hash widgets hooks config settings p in
       match ie with
       | Ok (_, []) -> Ok acc
       | Ok (_, new_pages) -> Ok (List.append new_pages acc)
       | Error _ as err -> err)
    []
    files

let process_asset_file settings src_path dst_path =
  let processor = find_preprocessor settings.asset_processors src_path in
  match processor with
  | None ->
    (* Utils.copy_file takes care of missing directories if needed. *)
    Utils.copy_file [src_path] dst_path
  | Some template ->
    (* Assets are processed early, so directories may not exist yet, we may need to create them. *)
    let () = FileUtil.mkdir ~parent:true dst_path in
    let file_name = FilePath.basename src_path in
    let jg_string = Jingoo.Jg_types.box_string in
    let env = [
      ("source_file_path", jg_string src_path);
      ("source_file_name", jg_string file_name);
      ("source_file_base_name", jg_string (file_name |> File_path.strip_extensions));
      ("target_dir", jg_string dst_path);
    ]
    in
    let command = Template.render template env in
    let () = Logs.info @@ fun m -> m {|Calling asset processor command: "%s"|} command in
    let res = Process_utils.get_program_output command in
    begin
      match res with
      | Ok output ->
        let () = Logs.debug @@ fun m -> m "Asset processor output:\n%s" output in
        Ok ()
      |
        (* XXX: This seemingly useless destructuring is there to keep the compiler happy
           because the type of Process_utils.get_program_output is p(string, string) result],
           while the type that Utils.iter_result expects is [(unit, string) result].
           Since [res] is [(string, string) result], we have to extract its error part
           and nominally "re-box" it.
         *)
        Error msg -> Error msg
    end

(* Prints a version message. *)
let print_version () =
  Printf.printf "soupault %s\n\n" Defaults.version_string;
  print_endline "Copyright 2023 Daniil Baturin et al.";
  print_endline "soupault is free software distributed under the MIT license.";
  print_endline "Visit https://www.soupault.app for news and documentation.";
  print_newline ();
  Printf.printf "Compiled with OCaml %s" Sys.ocaml_version;
  print_newline ()

let main cli_options =
  match cli_options.action with
  | ShowVersion ->
    let () = print_version () in
    exit 0
  | ShowVersionNumber ->
    let () = print_endline Defaults.version_string in
    exit 0
  | ShowDefaultConfig ->
    let () = print_endline (Project_init.make_default_config Defaults.default_settings) in
    exit 0
  | InitProject ->
    let settings = update_settings Defaults.default_settings cli_options in
    let () = Project_init.init settings in
    exit 0
  | ShowEffectiveConfig ->
    let* config, _, _, settings = initialize cli_options in
    let () = check_version settings in
    let () = Otoml.Printer.to_channel stdout config in
    exit 0
  | BuildWebsite ->
    let* config, widgets, hooks, settings = initialize cli_options in
    let () =
      Logs.info @@ fun m -> m "Starting website build";
      check_version settings;
      setup_logging settings.verbose settings.debug;
      if settings.build_profiles <> []
      then Logs.info @@ fun m -> m "Running with build profiles: %s" (String.concat ", " settings.build_profiles)
    in
    let* () = make_build_dir settings.build_dir in
    let (page_files, index_files, asset_files) = Site_dir.get_site_files settings in
    (* If settings.process_pages_first is set, extract those pages and move them to the head of the list.
       For an empty list it would return the original list, but it would require traversing that list twice,
       so it's better to avoid it unless it's actually required. *)
    let* page_files =
      if settings.process_pages_first <> []
      then Site_dir.reorder_pages settings page_files
      else Ok page_files
    in
    let* () =
      if not settings.index_only
      then Utils.iter_result (fun (src, dst) -> process_asset_file settings src dst) asset_files
      else Ok ()
    in
    (* A bit of code duplication ahead, for now at least...

       Page processing workflows with [index.index_first=true] and without is slightly different.
       The purpose of [index_first=true] is to make the entire site metadata available to _all_ pages.
       Obviously, it can only be done by doing certain amount of work twice:
       at the very least, reading all pages, running widgets that aren't in [index.extract_after_widgets],
       and extracting fields.
     *)
    if settings.index_first then
      (* Creates a random-access version of the site index from a list of entries. *)
      let import_index_hash hash entries =
        List.iter (fun e -> Hashtbl.add hash e.index_entry_page_file e) entries 
      in
      (* The user wants the complete site metadata available to widgets/plugins on every page. *)
      begin
        (* Make a random-access hash table version of the index to provide an index entry to widgets and
           hooks for the page being processed. *)
        let index_hash = Hashtbl.create 1024 in
        (* Do just enough work to have all site metadata produced and extracted.
           [index_only=true] prevents [process_page] from rendering pages and writing them to disk.

           It also often (though not always) reduces the number of widgets that will run on each page
           because widgets that aren't in the [extract_after_widgets] list are not processed.
         *)
        let* index = process_page_files index_hash widgets hooks config {settings with index_only=true} page_files in
        (* Sort entries according to the global settings so that widgets that use index data
           don't have to sort it themselves. *)
        let* index = Autoindex.sort_entries settings settings.index_sort_options index in
        let () = import_index_hash index_hash index in
       (* Since metadata extraction is already done and the complete site metadata should be available to all pages,
           content pages and section index pages should be treated the same.
           So we merge the lists of content and index pages back into one list
           and process it to generate the website.
         *)
        let all_files = List.append page_files index_files in
        (* Disable metadata extraction to avoid doing useless work, then process all pages.
           In practice, only index pages may produce new pages, but for simplicity we merge the lists
           because there's no harm in trying to collect generated pages from non-index pages,
           they will simply return empty lists.
         *)
        let settings = {settings with no_index_extraction=true} in
        (* At this step Lua index_processors may generate new pages, e.g. for taxonomies or pagination. *)
        let* new_pages = process_index_files index index_hash widgets hooks config settings all_files in
        (* Now process those "fake" pages generated by index processors.
           Index processing must be disabled on them to prevent index processors from generating
           new "fake" pages from generated pages and creating infinite loops.
         *)
        let settings = {settings with index=false} in
        let* () = Utils.iter_result (process_page index index_hash widgets hooks config settings) new_pages in
        (* Finally, dump the index file, if requested. *)
        let* () = dump_index_json settings index in
        Ok ()
      end
    else
      (* The user only wants site metadata available to section index pages
         and doesn't want the performance penalty of processing anything twice.
       *)
      begin
        (* Since in the [index_first=false] mode non-index pages have no access to the site-wide index data,
           we simply give the [process_page] function an empty hash.
         *)
        let index_hash = Hashtbl.create 1 in
        (* Process normal pages and collect index data from them.
           The [process_page_files] function is not using the [index_hash] argument,
           it's only needed to keep its underlying [process_page] call well-typed,
           so we can safely give it an empty hash.
         *)
        let* index = process_page_files index_hash widgets hooks config settings page_files in
        (* Sort entries according to the global settings so that widgets that use index data
           don't have to sort it themselves. *)
        let* index = Autoindex.sort_entries settings settings.index_sort_options index in
        (* Now process the index pages, using previously collected index data.
           That will not produce new index data because extraction will not run,
           but index processors may generate new pages (e.g. pagination and taxonomies).
         *)
        let settings = {settings with no_index_extraction=true} in
        let* new_pages = process_index_files index index_hash widgets hooks config settings index_files in
        (* Now process "fake" pages generated by index processors.
           Index processing must be disabled on them to prevent index processors from generating
           new "fake" pages from generated pages and creating infinite loops.
         *)
        let settings = {settings with index=false} in
        let* () = Utils.iter_result (process_page index index_hash widgets hooks config settings) new_pages in
        (* Finally, dump the index file, if requested. *)
        let* () = dump_index_json settings index in
        Ok ()
      end

let () =
  let cli_options = get_args () in
  let res = main cli_options in
  match res with
  | Ok _ -> exit 0
  | Error e ->
    Logs.err @@ fun m -> m "%s" e;
    exit 1
