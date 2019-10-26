module FU = FileUtil
module FP = FilePath

open Defaults

(* Windows doesn't understand normal newlines *)
let replace_newlines s =
  if Sys.os_type = "Win32" then Stringext.replace_all s ~pattern:"\n" ~with_:"\r\n"
  else s

let default_template = "
<html lang=\"en\">
  <head>
    <title> <!-- set automatically, see soupault.conf --> </title>
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  </head>
  <body>
    <!-- your page content will be inserted here,
         see the content_selector option in soupault.conf -->
  </body>
</html>
"

let default_page = "
<h1>Welcome!</h1>
<p>Welcome to my homepage. It's under construction.</p>
"

let default_config = "
# To learn about configuring soupalt, visit https://soupault.neocities.org/reference-manual
[settings]
  # Stop on page processing errors?
  strict = true

  # Display progress?
  verbose = false

  # Display detailed debug output?
  debug = false

  # Where to output pages
  build_dir = \"build\"

  # Where page content files are stored
  site_dir = \"site\"

  # Where page template is stored
  default_template = \"templates/main.html\"

  # Where to insert the page content inside the template
  content_selector = \"body\"

  doctype = \"<!DOCTYPE html>\"

  clean_urls = true

  # Files with these extensions are considered pages and processed
  # All other files are copied to build/ unchanged
  page_file_extensions = [\"htm\", \"html\", \"md\", \"rst\", \"adoc\"]

  # Files with these extensions are ignored
  ignore_extensions = [\"draft\"]

# Takes the content of the first <h1> and inserts it into the <title>
[widgets.page-title]
  widget = \"title\"
  selector = \"h1\"
  default = \"My Homepage\"
  append = \" &mdash; My Homepage\"

# Inserts a generator meta tag in the page <head>
# Just for demonstration, feel free to remove
[widgets.generator-meta]
  widget = \"insert_html\"
  html = '<meta name=\"generator\" content=\"soupault 1.4\">'
  selector = \"head\"
"

let print_end_message settings = Printf.printf "Initialization complete.

Now you can adjust the page layout in %s, add pages to the %s directory,
run soupault and find processed pages in the %s directory.\n"

settings.default_template settings.site_dir settings.build_dir

let init settings  =
  try
    print_endline "Initializing the project";
    (* Easier to just check from here than to pass another parameter
       around just for a single use in here *)
    if Config.config_exists Defaults.config_file
    then (print_endline "Config file exists, not overwriting it")
    else replace_newlines default_config |> Soup.write_file Defaults.config_file;

    if FileUtil.test (FileUtil.Is_dir) settings.site_dir then
      (print_endline "Site directory already exists. Are you running init in an existing project?";
      exit 1)
    else Printf.printf "Creating site directory %s\n" settings.site_dir; FileUtil.mkdir settings.site_dir;

    FU.mkdir settings.site_dir;
    replace_newlines default_page |> Soup.write_file (FP.concat settings.site_dir settings.index_file);

    FP.dirname settings.default_template |> FU.mkdir ~parent:true;
    replace_newlines default_template |> Soup.write_file settings.default_template;

    print_end_message settings
  with Unix.Unix_error (errno, _, _) ->
    let msg = Unix.error_message errno in
    let () = Printf.printf "Could not initialize the project directory: %s" msg in
    exit 1

