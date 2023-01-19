
type page_data = {
  page_file_path : string;
  page_content : string option;
  page_nav_path : string list;
}

type sort_type = Calendar | Numeric | Lexicographic

(** global settings that can be updated from config file or command line options *)
type index_field = {
  field_name : string;
  field_selectors : string list;
  select_all : bool;
  default_field_value : string option;
  extract_attribute : string option;
  fallback_to_content : bool; (* If extract_attribute cannot find the attribute *)
  required_field : bool;
}

type index_processor =
  | IndexTemplate of Template.t      (* Applied to the whole list of entries *)
  | IndexItemTemplate of Template.t  (* Applied to each entry separately *)
  | ExternalIndexer of string        (* External script, receives a JSON dump of the index *)
  | LuaIndexer of (string * string)  (* Lua plugin: file name and Lua source code *)

type path_options = {
  pages: string list;
  sections: string list;
  regexes: string list;
  pages_exclude: string list;
  sections_exclude: string list;
  regexes_exclude: string list;
  include_subsections: bool;
}

type sort_options = {
  sort_by : string option;
  sort_type : sort_type;
  sort_descending : bool;
  sort_strict : bool;
}

type index_view = {
  index_view_name : string;
  index_selector : string;
  index_action : string option;
  index_processor : index_processor;
  index_view_path_options : path_options;
  index_view_sort_options : sort_options option;
}

type index_entry = {
  index_entry_url: string;
  index_entry_page_file: string;
  index_entry_nav_path: string list;
  fields : (string * Ezjsonm.value) list;
}

type page_template = {
  template_name : string;
  template_data : string;
  template_path_options : path_options;
  template_content_selector : string option;
  template_content_action : string option;
}

type settings = {
  (* Show build progress. *)
  verbose : bool;

  (* Show debug information. *)
  debug : bool;

  (* Stop on page processing errors. *)
  strict : bool;

  (* HTML doctype to insert in generated pages. *)
  doctype : string;

  (* Whether to keep the doctype if a page already has one. *)
  keep_doctype : bool;

  (* Where generated pages are saved. *)
  build_dir : string;

  (* Where to look for page source files. *)
  site_dir : string;

  (* Page source file to use for section index, without extension. *)
  index_page : string;

  (* Generated section index file name. *)
  index_file : string;

  (* Default HTML page template. *)
  default_template : string;

  (* Default template source string (not a file path). *)
  default_template_source : string;

  (* What to do with the content. *)
  default_content_action : string;

  (* Alternative templates for specific pages. *)
  page_templates : page_template list;

  (* Element where page content is inserted in the template. *)
  default_content_selector : string;

  (* Use clean URLs if true (mirror the site dir structure exactly when false). *)
  clean_urls : bool;

  (* What files to consider pages rather than assets. *)
  page_extensions : string list;

  (* File extensions to ignore completely. *)
  ignore_extensions : string list;

  (* Extensions to keep intact when generating pages from content files.
     That's for people who want to use Markdown etc. _without_ also using clean URLs,
     so that about.htm remains about.htm, but contact.md becomes contact.html
   *)
  keep_extensions : string list;

  (* Extension to use for pages whose content file extension is _not_ in the keep_extensions list.
     E.g. a user has site/index.md, and it becomes build/index.html,
     while site/about.html can stay build/about.html
   *) 
  default_extension : string;

  (* HTML files considered complete pages rather than content files.
     Normally those that have an <html> element in them.
   *)
  complete_page_selector : string;

  (* If set to false, soupault doesn't use or require a page template,
     but treats everything as a complete page. *)
  generator_mode : bool;

  (* Content pages to process before everything else, if they exist. *)
  process_pages_first : string list;

  (* Build "profiles" specified from the CLI with --profile
     E.g. "dev" or "production".
     Widgets can be restricted to specific profiles.
   *)
  build_profiles : string list;

  (* Only extract the site index, don't generate any pages *)
  index_only : bool;

  (* Extract site index data from all pages
     and then process all pages again to make index data available
     to plugins.
   *)
  index_first : bool;

  (* Do not try to extract metadata from the page.
     This option is not visible to the user since it makes no sense alone
     (without disabling both extraction and insertion, like [index=false] does).
     It's only set internally to avoid useless work on the second pass
     when metadata is already extracted and we only need to generate output
     files from pages.
   *)
  no_index_extraction : bool;

  (* Enable site metadata extraction. *)
  index : bool;

  (* Save site metadata to a JSON file. *)
  dump_index_json : string option;


  ignore_template_errors : bool;

  (* The content model.
     Starting from 2.0.0 soupault doesn't have a built-in content model,
     users need to explicitly configure metadata field names
     and extraction rules (CSS selectors and extraction options).
   *)
  index_fields : index_field list;

  (* Widgets may generate data useful as metadata.
     In that case metadata extraction should be scheduled after those widgets have run.
   *)
  index_extract_after_widgets : string list;

  (* Strip HTML tags from extracted metadata. *)
  index_strip_tags : bool;

  (* Index views define different ways to present the metadata on site. *)
  index_views : index_view list;
  index_path_options : path_options;
  index_profile : string option;
  index_date_input_formats : string list;
  index_force : string list;
  index_leaf_file: string option;

  index_sort_options : sort_options;

  (* Page preprocessors convert other formats to HTML. *)
  page_preprocessors : (string * string) list;

  (* Asset processors that manipulate static asset files. *)
  asset_processors : (string * Template.t) list;

  plugin_dirs : string list;
  plugin_discovery: bool;

  caching: bool;
  cache_dir : string;

  force : bool;

  pretty_print_html : bool;

  soupault_version : string option;
}

type env = {
  nav_path : string list;
  page_file : string;
  page_url : string;
  target_dir : string;
  target_file : string;
  site_index : index_entry list;
  site_index_hash : (string, index_entry) Hashtbl.t;
  settings : settings;
}

let config_file = "soupault.toml"
let config_file_alt = "soupault.conf"
let config_path_env_var = "SOUPAULT_CONFIG"
let settings_table = "settings"
let index_settings_table = "index"
let page_preprocessors_table = "preprocessors"
let asset_processors_table = "asset_processors"
let widgets_table = "widgets"
let plugins_table = "plugins"
let templates_table = "templates"
let page_hash_file = ".page_source_hash"

let default_path_options = {
  pages = [];
  sections = [];
  regexes = [];
  pages_exclude = [];
  sections_exclude = [];
  regexes_exclude = [];
  include_subsections = false;
}

let default_sort_options = {
  sort_by = None;
  sort_type = Calendar;
  sort_strict = false;
  sort_descending = true;
}

let default_settings = {
  verbose = false;
  debug = false;
  strict = true;
  doctype = "<!DOCTYPE html>";
  keep_doctype = false;
  build_dir = "build";
  site_dir = "site";
  index_page = "index";
  index_file = "index.html";
  default_template = "templates/main.html";
  default_template_source = "";
  default_content_action = "append_child";
  page_templates = [];
  default_content_selector = "body";
  clean_urls = true;
  page_extensions = ["htm"; "html"; "md"; "rst"; "adoc"];
  ignore_extensions = [];
  default_extension = "html";
  keep_extensions = ["html"; "htm"];
  complete_page_selector = "html";
  generator_mode = true;
  process_pages_first = [];
  build_profiles = [];

  index = false;
  index_only = false;
  index_first = false;

  (* Must always be false by default, only used internally. *)
  no_index_extraction = false;

  dump_index_json = None;
  ignore_template_errors = false;
  index_extract_after_widgets = [];
  index_fields = [];
  index_strip_tags = false;
  index_views = [];
  index_path_options = default_path_options;
  index_profile = None;
  index_date_input_formats = ["%F"];
  index_force = [];
  index_leaf_file = None;

  index_sort_options = default_sort_options;

  page_preprocessors = [];
  asset_processors = [];

  plugin_dirs = ["plugins"];
  plugin_discovery = true;

  caching = false;
  cache_dir = ".soupault-cache";

  force = false;
  pretty_print_html = true;

  soupault_version = None;
}

let version = (4, 4, 0, None)

let version_to_string v =
  let major, minor, patch, suffix = v in
  match suffix with
  | Some suffix -> Printf.sprintf "%d.%d.%d-%s" major minor patch suffix
  | None -> Printf.sprintf "%d.%d.%d" major minor patch

let version_string = version_to_string version
