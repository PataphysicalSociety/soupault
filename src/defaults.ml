(** global settings that can be updated from config file or command line options *)
type settings = {
  (** show debug information *)
  verbose : bool;

  (** stop on page processing errors *)
  strict : bool;

  (** HTML doctype to insert in generated pages *)
  doctype : string;

  (** where generated pages are stored *)
  build_dir : string;

  (** where page source files are stored *)
  site_dir : string;

  (** page source file to use for section index, without extension *)
  index_page : string;

  (** generated section index file name *)
  index_file : string;

  (** default HTML page template *)
  default_template : string;

  (** element where page content is inserted in the template *)
  content_selector : string;

  (** use clean URLs or mirror the site dir structure exactly *)
  clean_urls : bool;

  index : bool;
  index_selector : string;
  index_title_selector : string;
  index_excerpt_selector : string;
  index_date_selector : string;
  index_author_selector : string;
  index_date_format : string;
  index_item_template : string;
  index_processor : string option;

  preprocessors : (string * string) list
}

type env = {
  template : string;
  nav_path : string list;
  page_file : string
}

let config_file = "soupault.conf"
let settings_table = "settings"
let widgets_table = "widgets"

let default_settings = {
  verbose = false;
  strict = true;
  doctype = "<!DOCTYPE html>";
  build_dir = "build";
  site_dir = "site";
  index_page = "index";
  index_file = "index.html";
  default_template = "templates/main.html";
  content_selector = "body";
  clean_urls = true;
  index = false;
  index_selector = "body";
  index_title_selector = "h1";
  index_excerpt_selector = "p";
  index_date_selector = "time";
  index_author_selector = "#author";
  index_date_format = "%F";
  index_item_template = "<div> </div>";
  index_processor = None;
  preprocessors = []
}
