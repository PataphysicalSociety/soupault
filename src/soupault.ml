open Soupault_common
open Defaults

let mkdir dir =
  (* Note: FileUtil.mkdir returns success if the directory
     already exists, this is why it's not checked before creation. *)
  try Ok (FileUtil.mkdir ~parent:true dir)
  with FileUtil.MkdirError e -> Error e

(*** Logging setup ***)

(* Determines whether log coloring is appropriate in the environment where soupault is running.

   As of now, soupault officially supports two kinds of OSes: UNIX-like and Windows.

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

   The default log format of the Daniel Bünzli's Logs library includes the name of the executable file in the message,
   which is useful in some situations, but I doubt it's helpful for typical soupault use cases —
   users knows what executable they run, or its full path is logged elsewhere.
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
  (* Enable exception tracing if debug=true,
     by default it's disabled in the OCaml runtime *)
  if debug then Printexc.record_backtrace true

(*** Filesystem stuff ***)

let list_dirs path =
    FileUtil.ls path |> FileUtil.filter FileUtil.Is_dir

let make_build_dir build_dir =
  if (FileUtil.test FileUtil.Exists build_dir) then () else
  let () = Logs.info @@ fun m -> m {|Build directory "%s" does not exist, creating|} build_dir in
  let res = mkdir build_dir in
  match res with
  | Ok () -> ()
  | Error msg ->
    Printf.ksprintf soupault_error "Failed to create build directory: %s" msg

(* Wrappers for running hooks *)
let run_pre_process_hook state hooks page_file target_dir target_file content =
  let settings = state.soupault_settings in
  let pre_process_hook = Hashtbl.find_opt hooks "pre-process" in
  match pre_process_hook with
  | Some (file_name, source_code, hook_config) ->
    if not (Hooks.hook_should_run settings hook_config "pre-process" page_file)
    then (target_dir, target_file, content)
    else
      Hooks.run_pre_process_hook
        state hook_config file_name source_code page_file target_dir target_file content
  | None -> (target_dir, target_file, content)

(*** Page processing helpers. ***)

(* Finds a preprocessor for given file name,
   if a preprocessor for its extension is configured.

   If a file has multiple extensions, soupault only consideres the last one
   (i.e., the extension of "index.en.md" for this purpose is "md", not "en").

   This function is used for determining both page preprocessors (in the "[preprocessors]" config section)
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

let load_html state hooks page_file =
  let settings = state.soupault_settings in
  let run_preprocessor prep page_file =
    (* If not, run the preprocessor to get the HTML source. *)
    let prep_cmd = Printf.sprintf "%s %s" prep (Filename.quote page_file) in
    let () = Logs.info @@ fun m -> m {|Calling page preprocessor "%s" on page %s|} (String.escaped prep) page_file in
    let output = Process_utils.get_program_output prep_cmd in
    match output with
    | Ok output ->
      output
    | Error msg ->
      Printf.ksprintf soupault_error "Failed to run page preprocessor '%s': %s"
        prep_cmd msg
  in
  let load_file page_preprocessor page_file =
    try
      match page_preprocessor with
      | None ->
        let page_source = Soup.read_file page_file in
        let () = Cache.refresh_page_cache settings page_file page_source in
        page_source
      | Some prep ->
        if not settings.caching then run_preprocessor prep page_file else
        (* Check if we have converted HTML source in cache.

          Since preprocessors get a page file path as an argument rather than receive HTML through stdin
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
        if Option.is_some cached then Option.get cached else
        (* If not, run the preprocessor to get the HTML source. *)
        let output = run_preprocessor prep page_file in
        (* Cache the object for future use. *)
        let () = Cache.cache_object settings page_file prep page_source output in
        output
    with Sys_error msg ->
      Printf.ksprintf soupault_error "Failed to load page %s: %s" page_file msg
  in
  let page_preprocessor = find_preprocessor settings.page_preprocessors page_file in
  let page_source = load_file page_preprocessor page_file in
  let pre_parse_hook = Hashtbl.find_opt hooks "pre-parse" in
  match pre_parse_hook with
  | Some (file_name, source_code, hook_config) ->
    if Hooks.hook_should_run settings hook_config "pre-parse" page_file then
      Hooks.run_pre_parse_hook state hook_config file_name source_code page_file page_source
    else page_source
  | None ->
    page_source

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
      (* If we are asked to always completely replace the doctype,
         we need to remove the original doctype declaration from the document.

         XXX: As of lambdasoup 0.7.2, there's no way to delete the doctype "element"
         (which isn't actually an element anyway),
         so we extract the <html> from the document tree and prepend a new doctype to it.
         That is, if the document even has <html> element to begin with — see below. *)
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
   if the render hook is not configured or the page is excluded from it.
 *)
let render_html state hooks page =
  let () = Logs.info @@ fun m -> m "Rendering page %s" page.page_file in
  let settings = state.soupault_settings in
  let hook = Hashtbl.find_opt hooks "render" in
  match hook with
  | Some (file_name, source_code, hook_config) ->
    if not (Hooks.hook_should_run settings hook_config "render" page.page_file)
    then render_html_builtin settings page.element_tree
    else
      let page_source = Hooks.run_render_hook state hook_config file_name source_code page in
      page_source
  | None ->
    render_html_builtin settings page.element_tree

(* Injects page content into a template.
   Where exactly it will insert the content is defined by the [content_selector] option in the template config,
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
   that means inserting page content loaded from a file into a template.
   Pages that are already complete documents are returned unchanged.
 *)
let make_page settings page =
  (* If generator mode is off, treat everything like a complete page *)
  if not settings.generator_mode then page else
  let page_wrapper_elem = Soup.select_one settings.complete_page_selector page.element_tree in
  (* If page file appears to be a complete page rather than a page body,
     just return it *)
  match page_wrapper_elem with
  | Some _ ->
    let () =
      if settings.generator_mode then
      Logs.debug @@ fun m -> m "File appears to be a complete page, not using the page template"
      (* In the HTML processor mode there's no question — everything is treated as a complete page *)
    in page
  | None ->
    let tmpl = List.find_opt
      (fun t -> (Path_options.page_included settings t.template_path_options settings.site_dir page.page_file) = true)
      settings.page_templates
    in
    let html, content_selector, content_action = (match tmpl with
      | None ->
        let () = Logs.info @@ fun m -> m "Using the default template for page %s" page.page_file in
        (Soup.parse settings.default_template_source,
         Some settings.default_content_selector,
         Some settings.default_content_action)
      | Some t ->
        let () = Logs.info @@ fun m -> m {|Using template "%s" for page %s|} t.template_name page.page_file in
        (Soup.parse t.template_data,
         t.template_content_selector,
         t.template_content_action))
    in
    let content_selector = Option.value ~default:settings.default_content_selector content_selector in
    let content_action = Option.value ~default:settings.default_content_action content_action in
    let res = include_content content_action content_selector html page.element_tree in
    begin match res with
    | Ok () ->  {page with element_tree=html}
    | Error msg ->
      Printf.ksprintf soupault_error "Failed to assemble page %s: %s" page.page_file msg
    end

(* Widget processing *)
let process_widgets state widget_list widget_hash page =
  let settings = state.soupault_settings in
  let index = state.site_index in
  let process_widget state widget_hash page widget_name =
    let widget = Hashtbl.find widget_hash widget_name in
    if not (Widgets.widget_should_run settings widget_name widget page.page_file) then () else
    let () = Logs.info @@ fun m -> m {|Processing widget "%s" on page %s|} widget_name page.page_file in
    try widget.func state widget.config index page
    with Widget_error msg | Config_error msg ->
      soupault_error @@ Printf.sprintf "Failed to process widget %s on page %s: %s"
        widget_name page.page_file msg
  in
  List.iter (process_widget state widget_hash page) widget_list

(* Removes index page's parent dir from its navigation path

    When clean URLs are used, the "navigation path" —
    the list of directories/sections that precede the page —
    is different from the real directory path for index pages.

    For example, the parent of the page at foo/bar/index.html is technically "bar".
    However, if the intended way to access that page is just
    https://example.com/foo/bar,
    then trying to use that definition for generating breadcrumbs and similar
    will create pages with links to themselves.
   
    The only way to deal with it I could find
    is to remove the last parent if the page is an index page.
 *)
let fix_nav_path settings path page_name =
  if page_name = settings.index_page then Utils.drop_tail path
  else path

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
  else FilePath.concat target_dir page_name

(*  Generates page file name.

    If clean URLs are used, it's always <target_dir>/<settings.index_file>

    If clean URLs are not used, then the base file name is preserved.
    The extension, however, is set to settings.default_extension,
    unless it's in the settings.keep_extensions list.

    The reason for this extension juggling is that people may use page preprocessors
    but not use clean URLs, without extension mangling they will end up
    with pages like build/about.md that have HTML inside despit their name.
    In short, that's what Jekyll et al. always did to non-blog pages.
 *)
let make_page_file_path settings page_file target_dir =
  if settings.clean_urls then (FilePath.concat target_dir settings.index_file) else
  let page_file = FilePath.basename page_file in
  let extension = File_path.get_extension page_file in
  let page_file =
    if Utils.in_list extension settings.keep_extensions then page_file
    else FilePath.add_extension (FilePath.chop_extension page_file) settings.default_extension
  in FilePath.concat target_dir page_file

let make_page_url settings nav_path orig_path target_dir page_file =
  let orig_page_file_name = FilePath.basename page_file in
  let target_page =
    if settings.clean_urls then
      begin
        let url = target_dir |> FilePath.basename in
        if settings.clean_url_trailing_slash then url ^ "/" else url
      end
    else make_page_file_path settings orig_page_file_name ""
  in
  let path =
    if ((FilePath.chop_extension orig_page_file_name) = settings.index_page) then orig_path
    else (List.append nav_path [target_page])
  in
  (* URL path should be absolute *)
  String.concat "/" path |> Printf.sprintf "/%s"

(* Assembles a page data structure. *)
let make_page_data state hooks page_file element_tree =
  let page_name = FilePath.basename page_file |> FilePath.chop_extension in
  (* The "navigation path" of a page is a list of its parent dirs
     excluding site_dir — e.g., ["pets", "cats"] for "site/pets/cats/fluffy.html"
   *)
  let orig_nav_path = FilePath.dirname page_file |> File_path.split_path |> CCList.drop 1 in
  (* With a caveat — see above in [fix_nav_path].
     We need to avoid index pages referring to themselves.
   *)
  let settings = state.soupault_settings in
  let nav_path = fix_nav_path settings orig_nav_path page_name in
  let orig_target_dir = make_page_dir_name settings (File_path.concat_path orig_nav_path) page_name |>
    FilePath.concat settings.build_dir
  in
  let target_file = make_page_file_path settings page_file orig_target_dir in
  let (target_dir, target_file, element_tree) =
    run_pre_process_hook state hooks page_file orig_target_dir target_file element_tree
  in
  let url = make_page_url settings nav_path orig_nav_path target_dir page_file in
  let page = {
    element_tree = element_tree;
    nav_path = nav_path;
    url = url;
    page_file = page_file;
    orig_target_dir = orig_target_dir;
    target_dir = target_dir;
    target_file = target_file
  }
  in
  page

let save_html state hooks page rendered_page_text =
  let write_page target_file page_text =
    let res = Utils.write_file target_file page_text in
    match res with
    | Ok () -> ()
    | Error msg ->
      Printf.ksprintf soupault_error "Failed to save the page generated from %s to %s: %s"
        page.page_file page.target_file msg
  in
  let settings = state.soupault_settings in
  (* Make sure the target directory exists before trying to write to it:
     when clean URLs are used and the page is a non-index page,
     the target directory (generated from its file name)
     certainly will not exist at this point yet.
   *)
  let () =
    begin match mkdir page.target_dir with
    | Ok () -> ()
    | Error msg ->
      Printf.ksprintf soupault_error "Failed to create target directory %s for page %s: %s"
        page.target_dir page.page_file msg
    end
  in
  let save_hook = Hashtbl.find_opt hooks "save" in
  match save_hook with
  | Some (file_name, source_code, hook_config) ->
    if Hooks.hook_should_run settings hook_config "save" page.page_file then
      Hooks.run_save_hook state hook_config file_name source_code page rendered_page_text
    else write_page page.target_file rendered_page_text
  | None ->
    let () = Logs.info @@ fun m -> m "Saving the page generated from %s to %s" page.page_file page.target_file in
    write_page page.target_file rendered_page_text

let extract_metadata state hooks page =
  (* Metadata is only extracted from non-index pages *)
  let settings = state.soupault_settings in
  if not (Autoindex.index_extraction_should_run settings page.page_file) then None else
  let entry = Autoindex.get_index_entry settings page in
  let post_index_hook = Hashtbl.find_opt hooks "post-index" in
  match post_index_hook with
  | Some (file_name, source_code, hook_config) ->
    if not (Hooks.hook_should_run settings hook_config "post-index" page.page_file) then (Some entry) else
    (* Let the post-index hook update the fields.
       It can also set a special [ignore_page] variable to tell soupault to exclude the page
       from indexing and any further processing.
     *)
    let index_fields =
      Hooks.run_post_index_hook state hook_config file_name source_code page entry
    in
    Some {entry with fields=index_fields}
  | None ->
    Some entry

(* Check if index insertion should be done and log the reason if not *)
let index_insertion_should_run settings index page_name =
  let aux settings page_name =
    if not settings.index then Some "indexing is disabled in the configuration" else
    if (page_name <> settings.index_page) then Some (Printf.sprintf "page name does not match %s" settings.index_page) else
    if index = [] then Some "index is empty" else None
  in
  match (aux settings page_name) with
  | None -> true
  | Some msg ->
    let () = Logs.debug @@ fun m -> m "Not inserting index data: %s" msg in
    false

(* Loads a page from a file on disk. *)
let load_page_file state hooks page_file =
  let () = Logs.info @@ fun m -> m "Loading page %s" page_file in
  let settings = state.soupault_settings in
  (* Load and parse page HTML. *)
  let html_source = load_html state hooks page_file in
  (* If we are in generator mode, tell the parser to interpret pages
     as HTML fragments that will go inside <body>,
     otherwise treat them as documents. *)
  let fragment = if settings.generator_mode then true else false in
  let element_tree = Html_utils.parse_page ~fragment:fragment settings html_source in
  let page_data = make_page_data state hooks page_file element_tree in
  page_data

let load_page_files state hooks files =
  List.map (load_page_file state hooks) files

(* Option parsing and initialization.

   One difficulty is that many CLI options can override values from the config file.
   Soupault stores the config internally as a record instead of querying a TOML datastructure all the time,
   since it's faster and more convenient. The TOML datastructure is only used by built-in widgets and Lua code.

   However, it also gives the user a way to view the effective config with --show-effective-config —
   that is, view all values as they are in the real soupault state: defaults, values from the config,
   and CLI overrides.
   It also strives to make overrides available to Lua code so that there is no disparity:
   built-in functionality and Lua code must see the same values.

   That requires a way to inject overrides into the TOML config.
 *)

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
  site_dir_opt: string option;
  build_dir_opt: string option;
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
  site_dir_opt = None;
  build_dir_opt = None;
  build_profiles_opt = [];
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
    ("--strict", Arg.Bool (fun _ -> ()), "<true|false>  Obsolete, has no effect");
    ("--site-dir", Arg.String (fun s -> opts := {!opts with site_dir_opt=(Some s)}), "<DIR>  Directory with input files");
    ("--build-dir", Arg.String (fun s -> opts := {!opts with build_dir_opt=(Some s)}), "<DIR>  Output directory");
    ("--profile", Arg.String (fun s -> opts := {!opts with build_profiles_opt=(s :: !opts.build_profiles_opt)}), "<NAME>  Build profile (you can give this option more than once)");
    ("--dump-index-json", Arg.String (fun s -> opts := {!opts with dump_index_json_opt=(Some s)}), "<FILE PATH>  Dump extracted index to a JSON file");
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
    if Option.is_some cli_options.force_opt then sr := {!sr with force=(Option.get cli_options.force_opt)};
    if Option.is_some cli_options.site_dir_opt then sr := {!sr with site_dir=(Option.get cli_options.site_dir_opt)};
    if Option.is_some cli_options.build_dir_opt then sr := {!sr with build_dir=(Option.get cli_options.build_dir_opt)};
    if Option.is_some cli_options.dump_index_json_opt then sr := {!sr with dump_index_json=cli_options.dump_index_json_opt};
    if Option.is_some cli_options.caching_opt then sr := {!sr with caching=(Option.get cli_options.caching_opt)};
    sr := {!sr with build_profiles=cli_options.build_profiles_opt}
  in !sr

let check_project_dir settings =
  let () =
    if (not (FileUtil.test FileUtil.Exists settings.default_template)) && settings.generator_mode then
    (* Don't make this fatal just yet, because:
         a) either it will blow up very soon after anyway, when soupault gets to the first page
         b) or ther user specified a custom template for every path.
     *)
    Logs.warn @@ fun m -> m {|Default template is required in generator mode, but template file "%s" does not exist|}
      settings.default_template
  in let () =
    if (not (FileUtil.test FileUtil.Is_dir settings.site_dir))
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
  | true, false -> Defaults.config_file
  | false, true -> Defaults.config_file_alt
  | true, true ->
    let () = Logs.warn @@ fun m -> m "Both %s and %s files exist, using %s"
      Defaults.config_file Defaults.config_file_alt Defaults.config_file
    in Defaults.config_file
  | false, false ->
      let () =
        Logs.err @@ fun m -> m "Could not find either %s or %s in the current directory."
          Defaults.config_file Defaults.config_file_alt;
        Logs.err @@ fun m -> m "Make sure you are in a soupault project directory or specify configuration file location in \
          %s environment variable." Defaults.config_path_env_var
      in
      Printf.ksprintf soupault_error "Cannot proceed without a configuration file."

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
  | Some path, None -> path
  | None, Some path -> path
  | Some _, Some path ->
    let () = Logs.warn @@ fun m -> m "Both SOUPAULT_CONFIG environment variable and --config option are given, using --config" in
    path
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
  let config_file = find_config_file cli_options in
  let config = Config.read_config config_file in
  (* First, populate the settings from the config file data. *)
  let settings = Config.update_settings settings config in
  let config = Option.get config in
  (* Then override options in it with values from command line arguments, if there are any. *)
  let settings = update_settings settings cli_options in
  (* Update the log level from the config and arguments  *)
  let () = setup_logging settings.verbose settings.debug in
  let () = check_project_dir settings in
  (* Inject defaults and updated values back into the TOML config
     to make the complete effective settings available to plugins and visible in --show-effective-config. *)
  let config = Config.inject_options settings config in
  let () = show_startup_message settings in
  let plugins = Plugins.get_plugins config in
  let widgets = Widgets.get_widgets settings config plugins settings.index_extract_after_widgets in
  let hooks = Hooks.get_hooks config in
  let default_template_str =
    if settings.generator_mode
    then
      begin match Utils.read_file settings.default_template with
      | Ok str -> str
      | Error msg ->
        Printf.ksprintf soupault_error "Failed to load default template: %s" msg
      end
    else ""
  in
  let settings = {settings with default_template_source=default_template_str} in
  let () =
    if not settings.generator_mode
    then Logs.info @@ fun m -> m "Running in HTML processor mode, not using page templates";
  in
  if settings.site_dir = ""
  then soupault_error  "site_dir must be a directory path, not an empty string"
  else if settings.build_dir = ""
  then soupault_error "build_dir must be a directory path, not an empty string"
  else (config, widgets, hooks, settings)

let dump_index_json settings index =
  match settings.dump_index_json with
  | None -> ()
  | Some f ->
    let () = Logs.info @@ fun m -> m "Exporting index data to JSON file %s" f in
    try Soup.write_file f @@ Autoindex.json_string_of_entries index
    with Sys_error msg ->
      Printf.ksprintf soupault_error "Failed to save index JSON data: %s" msg

let check_version settings =
  match settings.soupault_version with
  | None -> ()
  | Some v ->
    let res = Version.require_version v in
    match res with
    | Ok r ->
      if r then ()
      else begin
        Printf.printf "According to settings.soupault_version, this configuration file is for soupault %s\n" v;
        Printf.printf "You are running soupault version %s, older than required\n" Defaults.version_string;
        Printf.printf "To proceed, upgrade soupault to at least %s, or (at your own risk) \
        remove the soupault_version option from your configuration\n" v;
        exit 1
      end
    | Error msg ->
      begin
        Printf.printf "Could not check configuration compatibility with running soupault version: %s\n" msg;
        print_endline "Maybe your settings.soupault_version option is malformed?\n";
        exit 1
      end

(* Adjusts asset files paths
   in the case when the pre-process hook modified the target dir.
 *)
let reparent_asset_files pages asset_files =
  let reparent_asset new_dir old_dir (src_path, dst_path) =
    if not ((old_dir = dst_path) || (FilePath.is_subdir dst_path old_dir))
    then (src_path, dst_path)
    else begin
      let new_dst_path = CCString.replace ~sub:old_dir ~by:new_dir dst_path in
      let () = Logs.debug @@
        fun m -> m "Target directory for asset file %s is set to %s" src_path new_dst_path
      in
      (src_path, new_dst_path)
    end
  in
  let rec aux pages asset_files =
    match pages with
    | [] -> asset_files
    | p :: ps ->
      if p.orig_target_dir = p.target_dir
      then aux ps asset_files
      else aux ps
        (List.map (reparent_asset p.target_dir p.orig_target_dir) asset_files)
  in
  let () = Logs.debug @@ fun m -> m "Adjusting asset file paths when necessary" in
  aux pages asset_files

let process_asset_file settings src_path dst_path =
  let () = Logs.debug @@ fun m -> m "Processing asset file %s" src_path in
  let processor = find_preprocessor settings.asset_processors src_path in
  match processor with
  | None ->
    (* Utils.copy_file takes care of missing directories if needed. *)
    let res = Utils.copy_file [src_path] dst_path in
    begin match res with
    | Ok () -> ()
    | Error msg ->
      Printf.ksprintf soupault_error "Failed to copy asset file from %s to %s: %s"
        src_path dst_path msg
    end
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
    begin match res with
    | Ok output ->
      Logs.debug @@ fun m -> m "Asset processor output:\n%s" output
    | Error msg ->
      Printf.ksprintf soupault_error "Failed to run asset processor command '%s': %s"
        command msg
    end

(* Prints a version message. *)
let print_version () =
  Printf.printf "soupault %s\n\n" Defaults.version_string;
  print_endline "Copyright 2025 Daniil Baturin et al.";
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
    let (config, _, _, settings) = initialize cli_options in
    let () = check_version settings in
    let () = Otoml.Printer.to_channel stdout config in
    exit 0
  | BuildWebsite ->
    let (config, widget_data, hooks, settings) = initialize cli_options in
    let state = {
      soupault_settings = settings;
      soupault_config = config;
      site_index = []
    }
    in
    (* Clear cache if --force is given, to make it a cold build. *)
    let () =
      if settings.force then begin
        Logs.info @@ fun m -> m "Clearing cache";
        FileUtil.rm ~force:FileUtil.Force ~recurse:true [settings.cache_dir]
      end
    in
    let pre_index_widgets, post_index_widgets, widget_hash = widget_data in
    let () =
      Logs.info @@ fun m -> m "Starting website build";
      check_version settings;
      setup_logging settings.verbose settings.debug;
      if settings.build_profiles <> []
      then Logs.info @@ fun m -> m "Running with build profiles: %s" (String.concat ", " settings.build_profiles)
    in
    let () = make_build_dir settings.build_dir in
    let () = Hooks.run_startup_hook state hooks in
    let () = Logs.info @@ fun m -> m "Discovering website files in %s" settings.site_dir in
    let (page_files, asset_files) = Site_dir.get_site_files settings in
    let () = Logs.info @@ fun m -> m "Loading page files" in
    let page_sources = load_page_files state hooks page_files in
    let asset_files = reparent_asset_files page_sources asset_files in
    let () = Logs.info @@ fun m -> m "Processing asset files" in
    let () = List.iter (fun (src, dst) -> process_asset_file settings src dst) asset_files in
    let () = Logs.info @@ fun m -> m "Processing pages" in
    let pages = List.map (make_page settings) page_sources in
    (* Run widgets that are scheduled to run before index extraction,
       so that they may produce metadata that the user wants to extract.
       We run those widgets on all pages right away to keep things simpler,
       even though index files are not used as metadata sources
       and running those widgets on them early should not be important. *)
    let () =
      if (pre_index_widgets <> [])
      then Logs.info @@ fun m -> m "Running widgets scheduled to run before index extraction"
    in
    let () = List.iter (process_widgets state pre_index_widgets widget_hash) pages in
    (* Extract metadata . *)
    let () = Logs.info @@ fun m -> m "Extracting metadata from pages" in
    let index = List.fold_left
      (fun acc p ->
        let () = Logs.info @@ fun m -> m "Extracting metadata from page %s" p.page_file in
        let res = extract_metadata state hooks p in
        match res with
        | Some e ->
          let () = Logs.debug @@ fun m -> m "Index entry for page %s:\n%s"
            p.page_file (Autoindex.json_of_entry e |> Ezjsonm.to_string)
          in
          (e :: acc)
        | None -> acc)
      []
      pages
    in
    (* Sort entries according to the global settings sort option so that widgets that use index data
       don't have to sort it themselves. *)
    let index = Autoindex.sort_entries settings settings.index_sort_options index in
    (* Run index processors, on all pages, content and index alike.
       At this stage, index processor plugins may produce autogenerated pages
       (for example, paginated blog indices).
       This step produces an intermediate (page_file, element_tree) list.
     *)
    let state = {state with site_index=index} in
    let new_page_sources = List.fold_left
      (fun acc page ->
         let nps = Autoindex.insert_indices state page in
         List.append acc nps) [] pages
    in
    let new_pages = List.map (fun (path, etree) -> make_page_data state hooks path etree) new_page_sources in
    let new_pages = List.map (make_page settings) new_pages in
    (* Run the remaining widgets on all pages.
       We do it only after index insertion, so that widgets can modify
       HTML nodes that index processors may create. *)
    let () =
      if (post_index_widgets <> [])
      then Logs.info @@ fun m -> m "Running widgets scheduled to run after index extraction"
    in
    let () = List.iter (process_widgets state post_index_widgets widget_hash) pages in
    (* Now process "fake" pages generated by index processors.
       We only run widgets on them, since index data insertion was already done
       by the index processors that produced them. *)
    let all_widgets = List.append pre_index_widgets post_index_widgets in
    let () = List.iter (process_widgets state all_widgets widget_hash) new_pages in
    let () = Logs.info @@ fun m -> m "Rendering generated pages to HTML" in
    let all_pages = List.append pages new_pages in
    (* We need the original page record so that the save hook can use it,
       so we create a list of tuples. *)
    let generated_pages = List.map (fun p -> (p, render_html state hooks p)) all_pages in
    let () = Logs.info @@ fun m -> m "Saving generated pages to disk" in
    let () = List.iter
      (fun (p, pt) ->
         save_html state hooks p pt)
      generated_pages
    in
    (* Run the post-build hook — it runs once, after all pages are saved to disk. *)
    let () = Hooks.run_post_build_hook state index hooks in
    (* Finally, dump the index file, if requested. *)
    let () = dump_index_json settings index in
    Logs.info @@ fun m -> m "Build completed successfully"

let () =
  let cli_options = get_args () in
  try main cli_options
  with
  | Soupault_error e ->
    Logs.err @@ fun m -> m "%s" e;
    exit 1
