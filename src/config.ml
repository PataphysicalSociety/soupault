open Defaults

open Toml_utils

exception Config_error of string

let config_error err = raise (Config_error err)

let default d o = CCOpt.get_or ~default:d o

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
  "after"
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
  let keys = Toml_utils.list_table_keys config in
  List.iter (check_option valid_options) keys

(** Read and parse a TOML file *)
let read_config path =
  if not (config_exists path) then
    let () = Logs.warn @@ fun m -> m "Configuration file %s not found, using default settings" path in
    Ok None
  else
  try
    let open Toml.Parser in
    let conf = from_filename path |> unsafe in
    Ok (Some conf)
  with
  | Sys_error err -> Error (Printf.sprintf "Could not read config file: %s" err)
  | Toml.Parser.Error (err, _) -> Error (Printf.sprintf "Could not parse config file %s: %s" path err)

let get_table name config = field name config |> table
let get_table_result err name config = try Ok (get_table name config) with _ -> Error err
let get_table_opt name config = try Some (get_table name config) with _ -> None

let get_string ?(default=None) ?(strict=false) k tbl =
  let default = Option.map (fun b -> `String b) default in
  let res = field ~default:default k tbl in
  string ~strict:strict res

let get_string_result ?(strict=false) err k tbl = 
  try
    let res = get_string ~strict:strict k tbl in
    Ok res
  with
  | Key_error _ -> Error err

let get_string_opt ?(strict=false) k tbl = get_string_result ~strict:strict "" k tbl |> Result.to_option

let get_string_default default name config = get_string ~default:(Some default) name config

let get_bool ?(default=None) ?(strict=false) k tbl =
  let default = Option.map (fun b -> `Bool b) default in
  let res = field ~default:default k tbl in
  bool ~strict:strict res

let get_bool_default default name config = get_bool ~default:(Some default) name config

let get_int_default default_value k tbl = field ~default:(Some (`Float (float_of_int default_value))) k tbl |> number |> int_of_float


(** Tries to get a string list from a config
    If there's actually a list, converts every item to a string.
    If there's a single item, makes a single item list.
    If there's nothing like a string at all, returns the default value.
 *)
let get_strings_relaxed ?(default=([])) k tbl =
  let open Toml_utils in
  try field k tbl |> list ~strict:false |> strings ~strict:false
  with Key_error _ -> default

let assoc_of_table f t =
  match t with
  | `O os -> List.map (fun (k, v) -> (k, f v)) os
  | _ -> failwith ""

let get_path_options config =
  {
     pages = get_strings_relaxed "page" config;
     sections = get_strings_relaxed "section" config;
     regexes = get_strings_relaxed "path_regex" config;
     pages_exclude = get_strings_relaxed "exclude_page" config;
     sections_exclude = get_strings_relaxed "exclude_section" config;
     regexes_exclude = get_strings_relaxed "exclude_path_regex" config;
     include_subsections = get_bool_default false "include_subsections" config;
  }

let valid_path_options = [
    "page"; "section"; "path_regex"; "exclude_page"; "exclude_section"; "exclude_path_regex"
  ]

(* Update global settings with values from the config, if there are any *)
let _get_preprocessors config =
  let pt = get_table_opt Defaults.preprocessors_table config in
  match pt with
  | None -> []
  | Some pt -> assoc_of_table string pt

let get_index_queries index_table =
  let get_query k it =
    let qt =
      try get_table k it
      with Type_error e -> config_error e
    in
    let selectors = get_strings_relaxed "selector" qt in
    let default_value = get_string_opt "default" qt in
    let extract_attribute = get_string_opt "extract_attribute" qt in
    let content_fallback = get_bool_default false "fallback_to_content" qt in
    let select_all = get_bool_default false "select_all" qt in
    let () =
      if (Option.is_some default_value) && select_all then
      Logs.warn @@ fun m -> m "default is ignored when select_all is true"
    in
    (match selectors with
    | [] -> config_error "selector option is required and must be a string or a list of strings"
    | _ ->
      {
        field_name = k; field_selectors = selectors;
        select_all = select_all; default_field_value = default_value;
        extract_attribute = extract_attribute; fallback_to_content = content_fallback;
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
  let qt = get_table_opt "fields" index_table in
  match qt with
  | None -> []
  | Some qt ->
    get_queries qt (Toml_utils.list_table_keys qt) []

let valid_index_options = [
  "fields"; "views"; (* subtables rather than options *)
  "index"; "dump_json"; "sort_by"; "sort_descending";
  "ignore_template_errors"; "extract_after_widgets"; "strip_tags";
  "profile"
]

let valid_index_options = List.append valid_index_options valid_path_options

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
    let item_template = get_string_opt ~strict:true "index_item_template" st in
    let index_template = get_string_opt ~strict:true "index_template" st in
    let script = get_string_opt ~strict:true "index_processor" st in
    match item_template, index_template, script with
    | Some item_template, None, None -> _get_template item_template
    | None, Some index_template, None -> _get_template ~item_template:false index_template
    | None, None, Some script -> ExternalIndexer script
    | None, None, None ->
      let () = Logs.warn @@ fun m -> m "Index view \"%s\" does not have index_item_template, index_template, or index_processor option, using default template" view_name
      in default_index_processor
    | _ -> config_error "options index_item_template, index_template, and index_processor are mutually exclusive, please pick only one"
  in
  let selector = get_string "index_selector" st in
  let index_processor = _get_index_processor st in
  {
    index_view_name = view_name;
    index_selector = selector;
    index_processor = index_processor;
    index_view_path_options = (get_path_options st)
  }

let _get_index_views index_table =
  let get_view k views =
    let vt = get_table k views in
    _get_index_view vt k
  in
  let rec get_views ks views acc =
    match ks with
    | [] -> acc
    | k :: ks -> begin
      try
        let v = get_view k views in
        get_views ks views (v :: acc)
      with Config_error e | Type_error e ->
        Printf.ksprintf config_error "Misconfigured index view \"%s\": %s" k e
    end
  in
  let views = [] in
  let vt = get_table_opt "views" index_table in
  match vt with
  | None -> views
  | Some vt -> 
    let custom_views = get_views (Toml_utils.list_table_keys vt) vt [] in
    List.append views custom_views

let _get_index_settings settings config =
  let st = get_table_opt Defaults.index_settings_table config in
  match st with
  | None -> settings
  | Some st ->
    let () = check_options valid_index_options st "table \"index\"" in
    {settings with
       index = get_bool_default settings.index "index" st;
       dump_json = get_string_opt "dump_json" st;
       ignore_template_errors = get_bool_default settings.ignore_template_errors "ignore_template_errors" st;
       index_extract_after_widgets = get_strings_relaxed "extract_after_widgets" st;
       index_fields = get_index_queries st;
       index_strip_tags = get_bool_default settings.index_strip_tags "strip_tags" st;
       index_views = _get_index_views st;
       index_profile = get_string_opt "profile" st;
       index_path_options = get_path_options st;
       index_sort_by = get_string_opt "sort_by" st;
       index_sort_descending = get_bool_default true "sort_descending" st;
    }

let update_page_template_settings settings config =
  let get_template name settings config =
    (* Retrieve a subtable for given template *)
    let config = get_table_opt name config in
    match config with
    | None -> settings
    | Some config -> begin
      let path_options = get_path_options config in
      let file = get_string_opt "file" config in
      let content_selector = get_string_opt "content_selector" config in
      match file with
      | None -> Printf.ksprintf config_error "Missing required option \"file\" in [templates.%s]" name
      | Some file ->
        try
          let tmpl_data = Soup.read_file file in
          let tmpl = {
            template_name = name; template_data=tmpl_data;
            template_content_selector = content_selector;
            template_path_options = path_options
          }
          in
          let templates = tmpl :: settings.page_templates in
          {settings with page_templates=templates}
        with Sys_error msg ->
          Printf.ksprintf config_error "Could not load the file for [templates.%s]: %s, ignoring" name msg
    end
  in
  let tt = get_table_opt Defaults.templates_table config in
  match tt with
  | None -> settings
  | Some tt ->
    let ks = Toml_utils.list_table_keys tt in
    List.fold_left (fun s k -> get_template k s tt) settings ks

let valid_settings = [
  "verbose"; "debug"; "strict"; "site_dir"; "build_dir";
  "default_content_selector"; "doctype"; "index_page"; "index_file";
  "default_template"; "clean_urls"; "page_file_extensions";
  "ignore_extensions"; "default_extension"; "keep_extensions";
  "complete_page_selector"; "generator_mode";
  "plugin_dirs"; "plugin_discovery"
]

let _update_settings settings config =
  let st = get_table_opt Defaults.settings_table config in
  match st with
  | None ->
     let () = Logs.warn @@ fun m -> m "Could not find the [settings] section in the config, using defaults" in
     settings
  | Some st ->
    let () = check_options valid_settings st "table \"settings\"" in
    let settings = update_page_template_settings settings config in
    {settings with
       verbose = get_bool_default settings.verbose "verbose" st;
       debug = get_bool_default settings.debug "debug" st;
       strict = get_bool_default settings.strict "strict" st;
       site_dir = get_string_default settings.site_dir "site_dir" st |> String.trim;
       build_dir = get_string_default settings.build_dir "build_dir" st |> String.trim |> Utils.normalize_path;
       default_content_selector = get_string_default settings.default_content_selector "default_content_selector" st;
       doctype = get_string_default settings.doctype "doctype" st;
       index_page = get_string_default settings.index_page "index_page" st;
       index_file = get_string_default settings.index_file "index_file" st;
       default_template = get_string_default settings.default_template "default_template" st;
       clean_urls = get_bool_default settings.clean_urls "clean_urls" st;
       page_extensions = get_strings_relaxed ~default:settings.page_extensions "page_file_extensions" st;
       ignore_extensions = get_strings_relaxed ~default:[] "ignore_extensions" st;
       keep_extensions = get_strings_relaxed ~default:settings.keep_extensions "keep_extensions" st;
       default_extension = get_string_default settings.default_extension "default_extension" st;
       complete_page_selector = get_string_default settings.complete_page_selector "complete_page_selector" st;
       generator_mode = get_bool_default settings.generator_mode "generator_mode" st;

       plugin_dirs = get_strings_relaxed ~default:settings.plugin_dirs "plugin_dirs" st;
       plugin_discovery = get_bool_default settings.plugin_discovery "plugin_discovery" st;

       preprocessors = _get_preprocessors config
     }

let valid_tables = ["settings"; "index"; "plugins"; "widgets"; "preprocessors"; "templates"]

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
    let config = Toml_utils.json_of_table config in
    let () = check_options ~fmt:bad_section_msg valid_tables config "table \"settings\"" in
    let settings = _update_settings settings config in
    _get_index_settings settings config

let update_settings settings config =
  try Ok (update_settings_unsafe settings config)
  with Config_error e -> Error e
