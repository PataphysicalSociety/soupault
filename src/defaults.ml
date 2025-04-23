
type page_data = {
  page_file: string;
  element_tree: Soup.soup Soup.node;
  nav_path : string list;

  (* Target dir before its possible modification
     by the pre-parse hook. *)
  orig_target_dir: string;
  target_dir: string;
  target_file: string;
  url: string;
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
  max_items : int option;
}

type index_entry = {
  index_entry_url: string;
  index_entry_page_file: string;
  index_entry_nav_path: string list;
  fields : (string * Ezjsonm.value) list;
}

type index = index_entry list

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

  (* Whether to add a trailing slash to clean URLs.
     Many web servers redirect https://example.com/foo to https://example.com/foo/
     so URLs without trailing slashes can slow down page loading
     due to the time required to handle an HTTP redirect
     and may negatively impact search engine optimization.
     Soupault now adds a trailing slash by default,
     but gives the user an option to disable that.
    *)
  clean_url_trailing_slash : bool;

  (* What files to consider pages rather than assets. *)
  page_extensions : string list;

  (* Extensions for files to be processed by the built-in Markdown parser.
     If an extension is in this list, the file is processed
     by the built-in Markdown implementation
     and is not considered for external preprocessors.

     The list is empty by default,
     so that people can choose to use the built-in parser
     or an external preprocessor.
   *)
  markdown_extensions : string list;

  (* Files to ignore completely. *)
  ignore_extensions : string list;
  ignore_path_regexes: string list;
  ignore_directories: string list;

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

  (* Build "profiles" specified from the CLI with --profile
     E.g. "dev" or "production".
     Widgets can be restricted to specific profiles.
   *)
  build_profiles : string list;

  (* Enable site metadata extraction. *)
  index : bool;

  (* Save site metadata to a JSON file. *)
  dump_index_json : string option;

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

  page_character_encoding : Markup.Encoding.t;

  pretty_print_html : bool;

  soupault_version : string option;
}

type state = {
  soupault_settings: settings;
  soupault_config: Otoml.t;
  site_index: index_entry list;
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
  clean_url_trailing_slash = true;
  page_extensions = ["htm"; "html"];
  markdown_extensions = [];
  ignore_extensions = [];
  ignore_path_regexes = [];
  ignore_directories = [];
  default_extension = "html";
  keep_extensions = ["html"; "htm"];
  complete_page_selector = "html";
  generator_mode = true;
  build_profiles = [];

  index = false;
  dump_index_json = None;
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

  caching = true;
  cache_dir = ".soupault-cache";

  force = false;

  page_character_encoding = Markup.Encoding.utf_8;

  pretty_print_html = true;

  soupault_version = None;
}

let version = (5, 0, 0, None)

let version_to_string v =
  let major, minor, patch, suffix = v in
  match suffix with
  | Some suffix -> Printf.sprintf "%d.%d.%d-%s" major minor patch suffix
  | None -> Printf.sprintf "%d.%d.%d" major minor patch

let version_string = version_to_string version
