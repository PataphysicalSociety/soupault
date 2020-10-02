(** global settings that can be updated from config file or command line options *)
type index_field = {
  field_name : string;
  field_selectors : string list;
  select_all : bool;
  default_field_value : string option;
  extract_attribute : string option;
  fallback_to_content : bool; (* If extract_attribute cannot find the attribute *)
}

type index_processor =
  | IndexTemplate of Template.t      (* Applied to the whole list of entries *)
  | IndexItemTemplate of Template.t  (* Applied to each entry separately *)
  | ExternalIndexer of string        (* External script, receives a JSON dump of the index *)

type path_options = {
  pages: string list;
  sections: string list;
  regexes: string list;
  pages_exclude: string list;
  sections_exclude: string list;
  regexes_exclude: string list;
  include_subsections: bool;
}

type index_view = {
  index_view_name : string;
  index_selector : string;
  index_processor : index_processor;
  index_view_path_options : path_options;
}

type page_template = {
  template_name : string;
  template_data : string;
  template_path_options : path_options;
  template_content_selector : string option;
  template_content_action : string option;
}

type settings = {
  (* show processing steps *)
  verbose : bool;

  (* show debug information *)
  debug : bool;

  (* stop on page processing errors *)
  strict : bool;

  (* HTML doctype to insert in generated pages *)
  doctype : string;

  (* where generated pages are stored *)
  build_dir : string;

  (* where page source files are stored *)
  site_dir : string;

  (* page source file to use for section index, without extension *)
  index_page : string;

  (* generated section index file name *)
  index_file : string;

  (* default HTML page template *)
  default_template : string;

  (* Temporary place for the template source, now that the global env is gone *)
  default_template_source : string;

  (* what to do with the content *)
  default_content_action : string;

  (* alternative templates for specific pages *)
  page_templates : page_template list;

  (* element where page content is inserted in the template *)
  default_content_selector : string;

  (* use clean URLs or mirror the site dir structure exactly *)
  clean_urls : bool;

  (* Extensions of files assumed to be pages *)
  page_extensions : string list;

  (* File extensions to ignore completely *)
  ignore_extensions : string list;

  (* Extension to use for page files with non-standard extensions. *)
  default_extension : string;

  (* What extensions to consider "standard" for the purpose of setting extensions.
     That's for people who want to use Markdown etc. _without_ also using clean URLs,
     so that about.htm remains about.htm, but contact.md becomes contact.html
   *)
  keep_extensions : string list;

  (* Pages that should be just run through the widgets
      rather than inserted in the template *)
  complete_page_selector : string;

  (* If set to false, soupault doesn't use or require a page template,
     but treats everything as a complete page *)
  generator_mode : bool;

  build_profile : string option;

  (* Only extract the site index, don't generate any pages *)
  index_only : bool;

  index : bool;
  dump_json : string option;
  ignore_template_errors : bool;
  index_fields : index_field list;
  index_extract_after_widgets : string list;
  index_strip_tags : bool;
  index_views : index_view list;
  index_path_options : path_options;
  index_profile : string option;
  index_date_input_formats : string list;
  index_sort_by : string option;
  index_sort_descending : bool;

  preprocessors : (string * string) list;

  plugin_dirs : string list;
  plugin_discovery: bool;
}

type index_entry = {
  index_entry_url: string;
  index_entry_page_file: string;
  index_entry_nav_path: string list;
  fields : (string * Ezjsonm.value) list;
}

type env = {
  nav_path : string list;
  page_file : string;
  page_url : string;
  target_dir : string;
  site_index : index_entry list;
}

let config_file = "soupault.conf"
let config_path_env_var = "SOUPAULT_CONFIG"
let settings_table = "settings"
let index_settings_table = "index"
let preprocessors_table = "preprocessors"
let widgets_table = "widgets"
let plugins_table = "plugins"
let templates_table = "templates"

let default_index_item_template = "<div> <a href=\"{{url}}\">{{title}}</a> </div>"

let default_index_processor = IndexItemTemplate (Template.of_string default_index_item_template)

let default_path_options = {
  pages = [];
  sections = [];
  regexes = [];
  pages_exclude = [];
  sections_exclude = [];
  regexes_exclude = [];
  include_subsections = false;
}

let default_settings = {
  verbose = false;
  debug = false;
  strict = true;
  doctype = "<!DOCTYPE html>";
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
  build_profile = None;

  index = false;
  index_only = false;
  dump_json = None;
  ignore_template_errors = false;
  index_extract_after_widgets = [];
  index_fields = [];
  index_strip_tags = false;
  index_views = [];
  index_path_options = default_path_options;
  index_profile = None;
  index_date_input_formats = ["%F"];
  index_sort_by = None;
  index_sort_descending = true;

  preprocessors = [];

  plugin_dirs = ["plugins"];
  plugin_discovery = true;
}

let version = (2, 0, 0, None)

let version_to_string v =
  let major, minor, patch, suffix = v in
  match suffix with
  | Some suffix -> Printf.sprintf "%d.%d.%d-%s" major minor patch suffix
  | None -> Printf.sprintf "%d.%d.%d" major minor patch

let version_string = version_to_string version
