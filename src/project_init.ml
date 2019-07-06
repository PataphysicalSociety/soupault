module FU = FileUtil
module FP = FilePath

open Defaults

let default_template = "
<html>
  <head></head>
  <body>
  <!-- your page content here -->
  </body>
</html>
"

let default_page = "<p>Site powered by soupault</p>\n"

let default_config = "
# To learn about configuring soupalt, visit https://baturin.org/projects/soupault
[settings]
  strict = true
  verbose = false

  build_dir = \"build\"

  site_dir = \"site\"

  default_template = \"templates/main.html\"
  content_selector = \"body\"

  doctype = \"<!DOCTYPE html>\"
"

let init settings  =
  try
    (* Easier to just check from here than to pass another parameter
       around just for a single use in here *)
    if Config.config_exists Defaults.config_file
    then (print_endline "Config file exists, not overwriting it")
    else Soup.write_file ("soupault.conf") Defaults.config_file;

    if FileUtil.test (FileUtil.Is_dir) settings.site_dir then
      (print_endline "Site directory already exists. Are you running init in an existing project?";
      exit 1)
    else FileUtil.mkdir settings.site_dir;

    FU.mkdir settings.site_dir;
    Soup.write_file (FP.concat settings.site_dir settings.index_file) default_page;

    FilePath.dirname settings.default_template |> FU.mkdir ~parent:true;
    Soup.write_file settings.default_template default_template;

    print_endline "Initialized the project directory."
  with Unix.Unix_error (errno, _, _) ->
    let msg = Unix.error_message errno in
    let () = Printf.printf "Could not initialize the project directory: %s" msg in
    exit 1

