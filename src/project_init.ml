module FU = FileUtil
module FP = FilePath

open Defaults

let default_template = {|
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title> <!-- set automatically, see soupault.conf --> </title>
    <meta name="viewport" content="width=device-width, initial-scale=1">
  </head>
  <body>
    <!-- your page content will be inserted here,
         see the default_content_selector option in soupault.conf -->
  </body>
</html>
|}

let default_page = "
<h1>Welcome!</h1>
<p>Welcome to my homepage. It&apos;s under construction.</p>
"

(* This part contains the default settings and it's used in more than one place in the code *)
let make_default_config settings = Printf.sprintf {|
# To learn about configuring soupault, visit https://www.soupault.app/reference-manual

[settings]
  # Soupault version that the config was written/generated for
  # Trying to process this config with an older version will result in an error message
  soupault_version = "%s"

  # Stop on page processing errors?
  strict = true

  # Display progress?
  verbose = true

  # Display detailed debug output?
  debug = false

  # Where input files (pages and assets) are stored.
  site_dir = "%s"

  # Where the output goes
  build_dir = "%s"

  # Files inside the site/ directory can be treated as pages or static assets,
  # depending on the extension.
  #
  # Files with extensions from this list are considered pages and processed.
  # All other files are copied to build/ unchanged.
  #
  # Note that for formats other than HTML, you need to specify an external program
  # for converting them to HTML (see below).
  page_file_extensions = ["htm", "html", "md", "rst", "adoc"]

  # By default, soupault uses "clean URLs",
  # that is, $site_dir/page.html is converted to $build_dir/page/index.html
  # You can make it produce $build_dir/page.tml instead by changing this option to false
  clean_urls = true

  # If you set clean_urls=false,
  # file names with ".html" and ".htm" extensions are left unchanged.
  keep_extensions = ["html", "htm"]

  # All other extensions (".md", ".rst"...) are replaced, by default with ".html"
  default_extension = "html"

  # Page files with these extensions are ignored.
  ignore_extensions = ["draft"]

  # Soupault can work as a website generator or an HTML processor.
  #
  # In the "website generator" mode, it considers files in site/ page bodies
  # and inserts them into the empty page template stored in templates/main.html
  #
  # Setting this option to false switches it to the "HTML processor" mode
  # when it considers every file in site/ a complete page and only runs it through widgets/plugins.
  generator_mode = true

  # Files that contain an <html> element are considered complete pages rather than page bodies,
  # even in the "website generator" mode.
  # This allows you to use a unique layout for some pages and still have them processed by widgets.
  complete_page_selector = "html"

  # Website generator mode requires a page template (an empty page to insert a page body into).
  # If you use "generator_mode = false", this file is not required.
  default_template_file = "templates/main.html"

  # Page content is inserted into a certain element of the page template.
  # This option is a CSS selector that is used for locating that element.
  # By default the content is inserted into the <body>
  default_content_selector = "body"

  # You can choose where exactly to insert the content in its parent element.
  # The default is append_child, but there are more, including prepend_child and replace_content
  default_content_action = "append_child"

  # If a page already has a document type declaration, keep the declaration
  keep_doctype = true

  # If a page does not have a document type declaration, force it to HTML5
  # With keep_doctype=false, soupault will replace existing declarations with it too
  doctype = "<!DOCTYPE html>"

  # Insert whitespace into HTML for better readability
  # When set to false, the original whitespace (if any) will be preserved as is
  pretty_print_html = true

  # Plugins can be either automatically discovered or loaded explicitly.
  # By default discovery is enabled and the place where soupault is looking is the plugins/ subdirectory
  # in your project.
  # E.g., a file at plugins/my-plugin.lua will be registered as a widget named "my-plugin".
  plugin_discovery = true
  plugin_dirs = ["plugins"]

  # Soupault can cache outputs of external programs
  # (page preprocessors and preprocess_element widget commands).
  # It's disabled by default but you can enable it and configure the cache directory name/path
  caching = false
  cache_dir = ".soupault-cache"
|}

Defaults.version_string settings.site_dir settings.build_dir

(* This part contains example settings for a simple website,
   to give the user an idea what kind of functionality is available. *)
let sample_config = {|

# It is possible to store pages in any format if you have a program
# that converts it to HTML and writes it to standard output.
# Example:
#[preprocessors]
#  md = "cmark --unsafe --smart"
#  adoc = "asciidoctor -o -"

# Pages can be further processed with "widgets"

# Takes the content of the first <h1> and inserts it into the <title>
[widgets.page-title]
  widget = "title"
  selector = "h1"
  default = "My Homepage"
  append = " &mdash; My Homepage"

  # Insert a <title> in a page if it doesn't have one already.
  # By default soupault assumes if it's missing, you don't want it.
  force = false

# Inserts a generator meta tag in the page <head>
# Just for demonstration, feel free to remove
[widgets.generator-meta]
  widget = "insert_html"
  html = '<meta name="generator" content="soupault">'
  selector = "head"

# <blink> elements are evil, delete them all
[widgets.no-blink]
  widget = "delete_element"
  selector = "blink"

  # By default this widget deletes all elements matching the selector,
  # but you can set this option to false to delete just the first one
  delete_all = true
|}

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
    else ((make_default_config settings) ^ " " ^ sample_config) |> Soup.write_file Defaults.config_file;

    if FileUtil.test (FileUtil.Is_dir) settings.site_dir then
      (print_endline "Site directory already exists. Are you running init in an existing project?";
      exit 1)
    else Printf.printf "Creating site directory %s\n" settings.site_dir; FileUtil.mkdir settings.site_dir;

    FU.mkdir settings.site_dir;
    default_page |> Soup.write_file (FP.concat settings.site_dir settings.index_file);

    FP.dirname settings.default_template |> FU.mkdir ~parent:true;
    default_template |> Soup.write_file settings.default_template;

    print_end_message settings
  with Unix.Unix_error (errno, _, _) ->
    let msg = Unix.error_message errno in
    let () = Printf.printf "Could not initialize the project directory: %s" msg in
    exit 1
