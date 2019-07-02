type settings = {
  verbose : bool;
  strict : bool;
  doctype : string;
  build_dir : string;
  site_dir : string;
  index_page : string;
  index_file : string;
  default_template : string;
  content_selector : string
}

type env = {
  template : string;
  nav_path : string list
}

let config_file = "soupault.conf"
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
  content_selector = "body"
}
