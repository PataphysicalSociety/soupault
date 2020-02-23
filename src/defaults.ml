(** global settings that can be updated from config file or command line options *)
type index_field = {
  field_name : string;
  field_selector : string;
  select_all : bool;
  default_field_value : string option
}

type index_processor = BuiltInTemplate of Mustache.t | ExternalIndexer of string

type index_view = {
  index_view_name : string;
  index_selector : string;
  index_processor : index_processor;
}

type path_options = {
  pages: string list;
  sections: string list;
  regexes: string list;
  pages_exclude: string list;
  sections_exclude: string list;
  regexes_exclude: string list;
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

  (* element where page content is inserted in the template *)
  content_selector : string;

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
  newest_entries_first : bool;
  index_title_selector : string list;
  index_excerpt_selector : string list;
  index_date_selector : string list;
  index_author_selector : string list;
  index_date_format : string;
  ignore_template_errors : bool;
  index_custom_fields : index_field list;
  index_extract_after_widgets : string list;
  index_strip_tags : bool;
  index_views: index_view list;
  index_path_options: path_options;
  index_profile: string option;

  preprocessors : (string * string) list
}

type env = {
  template : string;
  nav_path : string list;
  page_file : string;
  page_url : string;
  target_dir : string;
}

let config_file = "soupault.conf"
let config_path_env_var = "SOUPAULT_CONFIG"
let settings_table = "settings"
let index_settings_table = "index"
let preprocessors_table = "preprocessors"
let widgets_table = "widgets"
let plugins_table = "plugins"

let default_index_item_template = "<div> <a href=\"{{url}}\">{{{title}}}</a> </div>"

let default_index_processor = BuiltInTemplate (Mustache.of_string default_index_item_template)

let default_path_options = {
  pages = [];
  sections = [];
  regexes = [];
  pages_exclude = [];
  sections_exclude = [];
  regexes_exclude = [];
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
  content_selector = "body";
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
  newest_entries_first = false;
  index_title_selector = ["h1"];
  index_excerpt_selector = ["p"];
  index_date_selector = ["time"];
  index_author_selector = ["#author"];
  index_date_format = "%F";
  ignore_template_errors = false;
  index_extract_after_widgets = [];
  index_custom_fields = [];
  index_strip_tags = false;
  index_views = [];
  index_path_options = default_path_options;
  index_profile = None;

  preprocessors = []
}

let version = (1, 9, 0)

let version_to_string v =
  let v1, v2, v3 = v in Printf.sprintf "%d.%d.%d" v1 v2 v3

let version_string = version_to_string version
