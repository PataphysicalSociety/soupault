module OH = Otoml.Helpers

open Defaults

open Otoml

exception Config_error of string

let config_error err = raise (Config_error err)

let sort_type_from_string s =
  match s with
  | "calendar" -> Calendar
  | "numeric" -> Numeric
  | "lexicographic" -> Lexicographic
  | _ -> Printf.ksprintf config_error "\"%s\" is not a valid value for the sort_type option. Choose either \"calendar\", \"numeric\" or \"lexicographic\"" s

(** Checks if config file exists.
    When config doesn't exist, soupault uses default settings,
    so this is considered a normal condition.

    When we cannot even check if it exists or not, however, something is clearly
    so wrong that there's no point in doing anything else.
    *)
let config_exists file =
  try
     FileUtil.test (FileUtil.Exists) file
  with Unix.Unix_error (errno, _, _) ->
    let msg = Unix.error_message errno in
    let () = Logs.warn @@ fun m -> m "Could not check if config file %s exists: %s" file msg in
    exit 1

let common_widget_options = [
  "widget";
  "path_regex"; "exclude_path_regex";
  "page"; "exclude_page";
  "section"; "exclude_section";
  "profile";
  "after";
  "disabled"
]

let bad_option_msg opt ident suggestion =
  let suggestion_msg =
    (match suggestion with
    | None -> ""
    | Some s -> Printf.sprintf "Did you mean \"%s\"?" s)
  in Printf.sprintf "Option \"%s\" is not valid for %s. %s" opt ident suggestion_msg

(** Checks for invalid config options *)
let check_options ?(fmt=bad_option_msg) valid_options config ident =
  let check_option valid_options opt =
    if not (List.exists ((=) opt) valid_options) then
    let index = Spellcheck.make_index valid_options in
    let suggestion = Spellcheck.get_suggestion index opt in
    config_error (fmt opt ident suggestion)
  in
  let keys = Otoml.list_table_keys config in
  List.iter (check_option valid_options) keys

(** Read and parse a TOML file *)
let read_config path =
  if not (config_exists path) then
    let () = Logs.warn @@ fun m -> m "Configuration file %s not found, using default settings" path in
    Ok None
  else
  try
    let conf = Otoml.Parser.from_file path in
    Ok (Some conf)
  with
  | Sys_error err -> Error (Printf.sprintf "Could not read config file: %s" err)
  | Otoml.Parse_error (pos, msg) ->
    let msg = Printf.sprintf "Could not parse config file %s: %s"
      path (Otoml.Parser.format_parse_error pos msg)
    in Error msg

(* Convenience accessor wrappers *)

let find_table path config = TomlTable (find config get_table path)
let find_table_opt path config = find_opt config get_table path |> Option.map (fun x -> TomlTable x)
let find_table_result err path config = find_opt config get_table path |> Option.to_result ~none:err

let find_result accessor config path =
  try Ok (Otoml.find config accessor path)
  with
  | Otoml.Key_error _ -> Error (Printf.sprintf "Missing required option %s" (Otoml.string_of_path path))
  | Otoml.Type_error msg -> Error (Printf.sprintf "Wrong type for option %s: %s" (Otoml.string_of_path path) msg)

let find_string_or ?(strict=false) ~default:default config path =
  OH.find_string_opt ~strict:strict config path |> Option.value ~default:default

let find_string_result ?(strict=false) config path =
  find_result (Otoml.get_string ~strict:strict) config path

let find_strings_or ~default:default config path =
  try OH.find_strings config path 
  with Key_error _ -> default

let find_strings_result ?(strict=false) config path =
  let get_strings = Otoml.get_array ~strict:false (Otoml.get_string ~strict:strict) in
  find_result get_strings config path 

let find_bool_or ?(strict=false) ~default:default config path =
  OH.find_boolean_opt ~strict:strict config path |> Option.value ~default:default

let find_bool_result ?(strict=false) config path =
  find_result (Otoml.get_boolean ~strict:strict) config path

let find_integer_or ?(strict=false) ~default:default config path =
  OH.find_integer_opt ~strict:strict config path |> Option.value ~default:default

let find_integer_result ?(strict=false) config path =
  find_result (Otoml.get_integer ~strict:strict) config path

let get_path_options config =
  {
     pages = find_strings_or ~default:[] config ["page"];
     sections = find_strings_or ~default:[] config ["section"];
     regexes = find_strings_or ~default:[] config ["path_regex"];
     pages_exclude = find_strings_or ~default:[] config ["exclude_page"];
     sections_exclude = find_strings_or ~default:[] config ["exclude_section"];
     regexes_exclude = find_strings_or ~default:[] config ["exclude_path_regex"];
     include_subsections = find_bool_or ~default:false config ["include_subsections"];
  }

let valid_path_options = [
    "page"; "section"; "path_regex"; "exclude_page"; "exclude_section"; "exclude_path_regex"
  ]

(* Update global settings with values from the config, if there are any *)
let _get_preprocessors config =
  let t = find_table_opt [Defaults.preprocessors_table] config in
  match t with
  | None -> []
  | Some t ->
    let t = get_table t in
    Utils.assoc_map get_string t

let get_index_queries index_table =
  let get_query k it =
    let () = Otoml.Printer.to_channel stdout it in
    let () = print_endline k in
    let selectors = find_strings_or ~default:[] it [k; "selector"] in
    let default_value = OH.find_string_opt it [k; "default"] in
    let extract_attribute = OH.find_string_opt it [k; "extract_attribute"] in
    let content_fallback = find_bool_or ~default:false it [k; "fallback_to_content"] in
    let select_all = find_bool_or ~default:false it [k; "select_all"] in
    let required = find_bool_or ~default:false it [k; "required"] in
    let () =
      if (Option.is_some default_value) && select_all then
      Logs.warn @@ fun m -> m "default is ignored when select_all is true"
    in
    (match selectors with
    | [] -> config_error "selector option is required and must be a string or a list of strings"
    | _ ->
      {
        field_name = k;
        field_selectors = selectors;
        select_all = select_all;
        default_field_value = default_value;
        extract_attribute = extract_attribute;
        fallback_to_content = content_fallback;
        required_field = required;
      })
  in
  let rec get_queries qt ks acc =
    match ks with
    | [] -> acc
    | k :: ks' ->
      let q =
        try get_query k qt
        with Config_error err ->
          Printf.ksprintf config_error "Malformed config for index field \"%s\": %s" k err
      in get_queries qt ks' (q :: acc)
  in
  let qt = find_table_opt ["fields"] index_table in
  match qt with
  | None -> []
  | Some qt ->
    get_queries qt (Otoml.list_table_keys qt) []

let valid_index_options = [
  "fields"; "views"; (* subtables rather than options *)
  "index"; "dump_json"; "sort_by"; "sort_descending"; "sort_type"; "strict_sort"; "date_formats";
  "ignore_template_errors"; "extract_after_widgets"; "strip_tags";
  "force_indexing_path_regex"; "leaf_file";
  "profile"
] @ valid_path_options

let valid_index_view_options = [
  "index_item_template"; "index_template"; "index_processor";
  "index_selector"; "include_subsections"
] @ valid_path_options

let _get_index_view st view_name =
  let _get_template ?(item_template=true) tmpl =
    begin
      try
        let t = Template.of_string tmpl in
        if item_template then IndexItemTemplate t
        else IndexTemplate t
      with _ ->
        let () =
          Logs.warn @@ fun m -> m "Failed to parse template \"%s\", using default (%s)" tmpl default_index_item_template
        in default_index_processor
    end
  in
  let _get_index_processor st =
    let item_template = OH.find_string_opt ~strict:true st ["index_item_template"] in
    let index_template = OH.find_string_opt ~strict:true st ["index_template"] in
    let script = OH.find_string_opt ~strict:true st ["index_processor"] in
    match item_template, index_template, script with
    | Some item_template, None, None -> _get_template item_template
    | None, Some index_template, None -> _get_template ~item_template:false index_template
    | None, None, Some script -> ExternalIndexer script
    | None, None, None ->
      let () = Logs.warn @@ fun m -> m "Index view \"%s\" does not have index_item_template, index_template, or index_processor option, using default template" view_name
      in default_index_processor
    | _ -> config_error "options index_item_template, index_template, and index_processor are mutually exclusive, please pick only one"
  in
  let selector = OH.find_string st ["index_selector"] in
  let index_processor = _get_index_processor st in
  {
    index_view_name = view_name;
    index_selector = selector;
    index_processor = index_processor;
    index_view_path_options = (get_path_options st)
  }

let _get_index_views index_table =
  let get_view k views =
    let vt = find_table [k] views in
    let () = check_options valid_index_view_options vt "an index view" in
    _get_index_view vt k
  in
  let rec get_views ks views acc =
    match ks with
    | [] -> acc
    | k :: ks -> begin
      try
        let v = get_view k views in
        get_views ks views (v :: acc)
      with Config_error e | Key_error e | Type_error e ->
        Printf.ksprintf config_error "Misconfigured index view \"%s\": %s" k e
    end
  in
  let views = [] in
  let vt = find_table_opt ["views"] index_table in
  match vt with
  | None -> views
  | Some vt -> 
    let custom_views = get_views (Otoml.list_table_keys vt) vt [] in
    List.append views custom_views

let _get_index_settings settings config =
  let st = find_table_opt [Defaults.index_settings_table] config in
  match st with
  | None -> settings
  | Some st ->
    let () = check_options valid_index_options st "table \"index\"" in
    let date_formats = find_strings_or ~default:settings.index_date_input_formats st ["date_formats"] in
    {settings with
       index = find_bool_or ~default:settings.index st ["index"];
       dump_json = OH.find_string_opt st ["dump_json"];
       ignore_template_errors = find_bool_or ~default:settings.ignore_template_errors st ["ignore_template_errors"];
       index_extract_after_widgets = find_strings_or ~default:[] st ["extract_after_widgets"];
       index_fields = get_index_queries st;
       index_strip_tags = find_bool_or ~default:settings.index_strip_tags st ["strip_tags"];
       index_views = _get_index_views st;
       index_profile = OH.find_string_opt st ["profile"];
       index_path_options = get_path_options st;
       index_sort_by = OH.find_string_opt st ["sort_by"];
       index_sort_type = find_string_or ~default:"calendar" st ["sort_type"] |> sort_type_from_string;
       index_sort_strict = find_bool_or ~default:settings.index_sort_strict st ["strict_sort"];
       index_sort_descending = find_bool_or ~default:true st ["sort_descending"];
       index_date_input_formats = date_formats;
       index_force = find_strings_or ~default:[] st ["force_indexing_path_regex"];
       index_leaf_file = OH.find_string_opt st ["leaf_file"];
    }

let update_page_template_settings settings config =
  let get_template name settings config =
    (* Retrieve a subtable for given template *)
    let config = find_table_opt [name] config in
    match config with
    | None -> settings
    | Some config -> begin
      let path_options = get_path_options config in
      let file = OH.find_string_opt config ["file"] in
      let content_selector = OH.find_string_opt config ["content_selector"] in
      let content_action = OH.find_string_opt config ["content_action"] in
      match file with
      | None -> Printf.ksprintf config_error "Missing required option \"file\" in [templates.%s]" name
      | Some file ->
        try
          let tmpl_data = Soup.read_file file in
          let tmpl = {
            template_name = name; template_data=tmpl_data;
            template_content_selector = content_selector;
            template_content_action = content_action;
            template_path_options = path_options
          }
          in
          let templates = tmpl :: settings.page_templates in
          {settings with page_templates=templates}
        with Sys_error msg ->
          Printf.ksprintf config_error "Could not load the file for [templates.%s]: %s, ignoring" name msg
    end
  in
  let tt = find_table_opt [Defaults.templates_table] config in
  match tt with
  | None -> settings
  | Some tt ->
    let ks = Otoml.list_table_keys tt in
    List.fold_left (fun s k -> get_template k s tt) settings ks

let valid_settings = [
  "verbose"; "debug"; "strict"; "site_dir"; "build_dir";
  "default_content_selector"; "default_template_file"; "default_content_action"; 
  "doctype"; "keep_doctype";
  "index_page"; "index_file";
  "clean_urls"; "page_file_extensions";
  "ignore_extensions"; "default_extension"; "keep_extensions";
  "complete_page_selector"; "generator_mode";
  "plugin_dirs"; "plugin_discovery";
  "force"; "pretty_print_html";
  "soupault_version";
]

let _update_settings settings config =
  let st = find_table_opt [Defaults.settings_table] config in
  match st with
  | None ->
     let () = Logs.warn @@ fun m -> m "Could not find the [settings] section in the config, using defaults" in
     settings
  | Some st ->
    let () = check_options valid_settings st "table \"settings\"" in
    let settings = update_page_template_settings settings config in
    {settings with
       verbose = find_bool_or ~default:settings.verbose st ["verbose"];
       debug = find_bool_or ~default:settings.debug st ["debug"];
       strict = find_bool_or ~default:settings.strict st ["strict"];
       site_dir = find_string_or ~default:settings.site_dir st ["site_dir"] |> String.trim;
       build_dir = find_string_or ~default:settings.build_dir st ["build_dir"] |> String.trim |> Utils.normalize_path;
       default_content_selector = find_string_or ~default:settings.default_content_selector st ["default_content_selector"];
       doctype = find_string_or ~default:settings.doctype st ["doctype"];
       keep_doctype = find_bool_or ~default:settings.keep_doctype st ["keep_doctype"];
       index_page = find_string_or ~default:settings.index_page st ["index_page"];
       index_file = find_string_or ~default:settings.index_file st ["index_file"];
       default_template = find_string_or ~default:settings.default_template st ["default_template_file"];
       default_content_action = find_string_or ~default:settings.default_content_action st ["default_content_action"];
       clean_urls = find_bool_or ~default:settings.clean_urls st ["clean_urls"];
       page_extensions = find_strings_or ~default:settings.page_extensions st ["page_file_extensions"];
       ignore_extensions = find_strings_or ~default:[] st ["ignore_extensions"];
       keep_extensions = find_strings_or ~default:settings.keep_extensions st ["keep_extensions"];
       default_extension = find_string_or ~default:settings.default_extension st ["default_extension"];
       complete_page_selector = find_string_or ~default:settings.complete_page_selector st ["complete_page_selector"];
       generator_mode = find_bool_or ~default:settings.generator_mode st ["generator_mode"];

       plugin_dirs = find_strings_or ~default:settings.plugin_dirs st ["plugin_dirs"];
       plugin_discovery = find_bool_or ~default:settings.plugin_discovery st ["plugin_discovery"];

       force = find_bool_or ~default:settings.force st ["force"];
       pretty_print_html = find_bool_or ~default:settings.pretty_print_html st ["pretty_print_html"];

       soupault_version = OH.find_string_opt st ["soupault_version"];

       preprocessors = _get_preprocessors config
     }

let valid_tables = ["settings"; "index"; "plugins"; "widgets"; "preprocessors"; "templates"; "custom_options"]

let update_settings_unsafe settings config =
  let bad_section_msg tbl _ suggestion =
    (* Yay, duplicate code! *)
    let suggestion_msg =
      (match suggestion with
      | None -> ""
      | Some s -> Printf.sprintf "Did you mean [%s]?" s)
    in Printf.sprintf "[%s] is not a valid config section. %s" tbl suggestion_msg
  in
  match config with
  | None -> settings
  | Some config ->
    let () = check_options ~fmt:bad_section_msg valid_tables config "table \"settings\"" in
    let settings = _update_settings settings config in
    _get_index_settings settings config

let update_settings settings config =
  try Ok (update_settings_unsafe settings config)
  with Config_error e -> Error e

let inject_default path default config =
  let value = find_opt config (fun x -> x) path in
  match value with
  | Some _ -> config
  | None -> update config path (Some default)

let inject_defaults settings config =
  let inject_default_settings settings config =
      inject_default ["settings"; "verbose"] (boolean settings.verbose) config |>
      inject_default ["settings"; "debug"] (boolean settings.debug) |>
      inject_default ["settings"; "strict"] (boolean settings.strict) |>
      inject_default ["settings"; "doctype"] (string settings.doctype) |>
      inject_default ["settings"; "keep_doctype"] (boolean settings.keep_doctype) |>
      inject_default ["settings"; "build_dir"] (string settings.build_dir) |>
      inject_default ["settings"; "site_dir"] (string settings.site_dir) |>
      inject_default ["settings"; "index_page"] (string settings.index_page) |>
      inject_default ["settings"; "index_file"] (string settings.index_file) |>
      inject_default ["settings"; "default_template"] (string settings.default_template) |>
      inject_default ["settings"; "default_content_action"] (string settings.default_content_action) |>
      inject_default ["settings"; "default_content_selector"] (string settings.default_content_selector) |>
      inject_default ["settings"; "clean_urls"] (boolean settings.clean_urls) |>
      inject_default ["settings"; "page_file_extensions"] (array @@ List.map string settings.page_extensions) |>
      inject_default ["settings"; "ignore_extensions"] (array []) |>
      inject_default ["settings"; "default_extension"] (string settings.default_extension) |>
      inject_default ["settings"; "keep_extensions"] (array @@ List.map string settings.keep_extensions) |>
      inject_default ["settings"; "complete_page_selector"] (string settings.complete_page_selector) |>
      inject_default ["settings"; "generator_mode"] (boolean settings.generator_mode) |>
      inject_default ["settings"; "pretty_print_html"] (boolean settings.pretty_print_html) |>
      inject_default ["settings"; "plugin_discovery"] (boolean settings.plugin_discovery) |>
      inject_default ["settings"; "plugin_dirs"] (array @@ List.map string settings.plugin_dirs)
  in
  let inject_default_index_settings settings config =
      inject_default ["index"; "index"] (boolean settings.index) config |>
      inject_default ["index"; "ignore_template_errors"] (boolean settings.ignore_template_errors) |>
      inject_default ["index"; "extract_after_widgets"] (array []) |>
      inject_default ["index"; "strip_tags"] (boolean settings.index_strip_tags) |>
      inject_default ["index"; "date_formats"] (array @@ List.map string settings.index_date_input_formats) |>
      inject_default ["index"; "sort_type"] (string "calendar") |>
      inject_default ["index"; "strict_sort"] (boolean settings.index_sort_strict) |>
      inject_default ["index"; "sort_descending"] (boolean settings.index_sort_descending) |>
      inject_default ["index"; "force_indexing_path_regex"] (array [])
  in
  let res = inject_default_settings settings config |> inject_default_index_settings settings in
  res
