open Defaults

let default d o = CCOpt.get_or ~default:d o

(* List all keys of a TOML table
   This is used to retrieve a list of widgets to call
 *)
let list_config_keys table =
  TomlTypes.Table.fold (fun k _ ks -> (TomlTypes.Table.Key.to_string k) :: ks ) table []

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
    Logs.warn @@ fun m -> m "%s" (fmt opt ident suggestion)
  in
  let keys = list_config_keys config in
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

let get_table name config = TomlLenses.(get config (key name |-- table))
let get_table_result err name config = get_table name config |> CCOpt.to_result err

let get_string k tbl = TomlLenses.(get tbl (key k |-- string))
let get_string_default default_value k tbl = get_string k tbl |> default default_value
let get_string_result err k tbl = get_string k tbl |> CCOpt.to_result err

let get_bool k tbl = TomlLenses.(get tbl (key k |-- bool))
let get_bool_default default_value k tbl = get_bool k tbl |> default default_value
let get_bool_result err k tbl = get_bool k tbl |> CCOpt.to_result err

let get_int k tbl = TomlLenses.(get tbl (key k |-- int))
let get_int_default default_value k tbl = get_int k tbl |> default default_value
let get_int_result err k tbl = get_int k tbl |> CCOpt.to_result err

let get_strings k tbl = TomlLenses.(get tbl (key k |-- array |-- strings))
let get_strings_default default_value k tbl = get_strings k tbl |> default default_value
let get_strings_result err k tbl = get_strings k tbl |> CCOpt.to_result err

(* For passing options to plugins *)
let get_whatever_as_string k tbl =
  (* A "maybe not" "monad" *)
  let (>>=) v f =
    match v with
    | None -> f v
    | Some _ as v -> v
  in
  get_string k tbl >>=
  (fun _ -> get_int k tbl |> CCOpt.map string_of_int) >>=
  (fun _ -> get_bool k tbl |> CCOpt.map string_of_bool)

let get_whatever k tbl =
  match (get_string k tbl) with
  | Some s -> `String s
  | None ->
    (match (get_int k tbl) with
    | Some i -> `Int i
    | None ->
      (match (get_bool k tbl) with
       | Some b -> `Bool b
       | None ->
         (match (get_strings k tbl) with
          | Some ss -> `A (List.map (fun x -> `String x) ss)
          | None -> `Null)))

(** Converts a TOML table to an assoc list using a given value retrieval function,
    ignoring None's it may return.
  *)
let assoc_of_table f tbl =
  let has_value (_, v) = match v with Some _ -> true | None -> false in
  let keys = list_config_keys tbl in
  List.fold_left (fun xs k -> (k, f k tbl ) :: xs) [] keys |>
  List.filter has_value |>
  List.map (fun (k, v) -> k, Option.get v)

let assoc_of_table2 f tbl =
  let keys = list_config_keys tbl in
  List.fold_left (fun xs k -> (k, f k tbl ) :: xs) [] keys

(** Tries to get a string list from a config
    If there's actually a string list, just returns it.
    If there's a single string, considers it a single item list.
    If there's nothing like a string at all, return the default.
 *)
let get_strings_relaxed ?(default=[]) k tbl =
  let strs = get_strings k tbl in
  match strs with
  | Some strs -> strs
  | None -> begin
      let str = get_string k tbl in
      match str with
      | Some str -> [str]
      | None -> default
    end

let string_of_assoc xs =
  let xs = List.map (fun (k, v) -> Printf.sprintf "%s = %s" k v) xs in
  String.concat ", " xs

let get_path_options config =
  {
     pages = get_strings_relaxed "page" config;
     sections = get_strings_relaxed "section" config;
     regexes = get_strings_relaxed "path_regex" config;
     pages_exclude = get_strings_relaxed "exclude_page" config;
     sections_exclude = get_strings_relaxed "exclude_section" config;
     regexes_exclude = get_strings_relaxed "exclude_path_regex" config
  }

let valid_path_options = [
    "page"; "section"; "path_regex"; "exclude_page"; "exclude_section"; "exclude_path_regex"
  ]

(* Update global settings with values from the config, if there are any *)
let _get_preprocessors config =
  let pt = get_table Defaults.preprocessors_table config in
  match pt with
  | None -> []
  | Some pt -> assoc_of_table get_string pt

let _get_index_queries index_table =
  let (let*) = Stdlib.Result.bind in
  let get_query k queries =
    let* qt = get_table_result "value is not an inline table" k queries in
    let selectors = get_strings_relaxed "selector" qt in
    let default_value = get_string "default" qt in
    let extract_attribute = get_string "extract_attribute" qt in
    let select_all = get_bool_default false "select_all" qt in
    let () =
      if (Option.is_some default_value) && select_all then
      Logs.warn @@ fun m -> m "default is ignored when select_all is true"
    in
    (match selectors with
    | [] -> Error "selector option is required and must be a string or a list of strings"
    | _ ->
      Ok {
        field_name = k; field_selectors = selectors;
        select_all = select_all; default_field_value = default_value;
        extract_attribute = extract_attribute;
      })
  in
  let rec get_queries ks queries acc =
    match ks with
    | [] -> acc
    | k :: ks ->
      begin
        let q = get_query k queries in
        match q with
        | Error e ->
          let () = Logs.warn @@ fun m -> m "Malformed index field query \"%s\": %s" k e in
          get_queries ks queries acc
        | Ok q -> get_queries ks queries (q :: acc)
      end
  in
  let qt = get_table "custom_fields" index_table in
  match qt with
  | None -> []
  | Some qt -> get_queries (list_config_keys qt) qt []

let valid_index_options = [
  "custom_fields"; "views"; (* subtables rather than options *)
  "index"; "dump_json"; "newest_entries_first";
  "index_selector"; "index_title_selector"; "index_excerpt_selector";
  "index_date_selector"; "index_author_selector";
  "index_date_format"; "index_item_template"; "index_processor";
  "ignore_template_errors"; "extract_after_widgets"; "strip_tags";
  "use_default_view"; "profile"
]

let valid_index_options = List.append valid_index_options valid_path_options

let _get_index_view st view_name =
  let _get_template tmpl =
    begin
      try BuiltInTemplate (Mustache.of_string tmpl)
      with _ ->
        let () = Logs.warn @@ fun m -> m "Failed to parse template \"%s\", using default (%s)" tmpl default_index_item_template in
        default_index_processor
    end
  in
  let _get_index_processor st =
    let template = get_string "index_item_template" st in
    let script = get_string "index_processor" st in
    match template, script with
    | Some template, None -> _get_template template
    | None, Some command -> ExternalIndexer command
    | Some _, Some command ->
      let () = Logs.warn @@ fun m -> m "Index view \"%s\": Found both index_item_template and index_processor options, using index_processor" view_name in
      ExternalIndexer command
    | None, None ->
      let () = Logs.warn @@ fun m -> m "Index view \"%s\": neither index_item_template nor index_processor are defined, using default template" view_name in
      default_index_processor
  in
  let selector = get_string_default "body" "index_selector" st in
  {index_view_name = view_name; index_selector = selector; index_processor = (_get_index_processor st)}

let _get_index_views index_table =
  let (let*) = Stdlib.Result.bind in
  let get_view k views =
    let* vt = get_table_result "value is not an inline table" k views in
    (* Stricter validation for non-default views *)
    let* _ = get_string_result (Printf.sprintf "Index view \"%s\": Missing required option \"selector\"" k) "index_selector" vt in
    (* Since getting an index view never actually fails due to backwards-compatibility with
       original options that all have defaults. *)
    Ok (_get_index_view vt k)
  in
  let rec get_views ks views acc =
    match ks with
    | [] -> acc
    | k :: ks ->
      begin
        let v = get_view k views in
        match v with
        | Error e ->
          let () = Logs.warn @@ fun m -> m "Malformed index field query \"%s\": %s" k e in
          get_views ks views acc
        | Ok q -> get_views ks views (q :: acc)
      end
  in
  (* In case someone wants to have _only_ custom views and not old style options *)
  let use_default_view = get_bool_default true "use_default_view" index_table in
  let views =
    if use_default_view then [_get_index_view index_table "default"]
    else []
  in
  let vt = get_table "views" index_table in
  match vt with
  | None -> views
  | Some vt -> 
    let custom_views = get_views (list_config_keys vt) vt [] in
    List.append views custom_views

let _get_index_settings settings config =
  let st = get_table Defaults.index_settings_table config in
  match st with
  | None -> settings
  | Some st ->
    let () = check_options valid_index_options st "table \"index\"" in
    {settings with
       index = get_bool_default settings.index "index" st;
       dump_json = get_string "dump_json" st;
       newest_entries_first = get_bool_default settings.newest_entries_first "newest_entries_first" st;
       index_title_selector = get_strings_relaxed ~default:settings.index_title_selector "index_title_selector" st;
       index_excerpt_selector = get_strings_relaxed ~default:settings.index_excerpt_selector "index_excerpt_selector" st;
       index_date_selector = get_strings_relaxed ~default:settings.index_date_selector "index_date_selector" st;
       index_author_selector = get_strings_relaxed ~default:settings.index_author_selector "index_author_selector" st;
       index_date_format = get_string_default settings.index_date_format "index_date_format" st;
       ignore_template_errors = get_bool_default settings.ignore_template_errors "ignore_template_errors" st;
       index_extract_after_widgets = get_strings_relaxed "extract_after_widgets" st;
       index_custom_fields = _get_index_queries st;
       index_strip_tags = get_bool_default settings.index_strip_tags "strip_tags" st;
       index_views = _get_index_views st;
       index_profile = get_string "profile" st;
       index_path_options = get_path_options st;
    }

let update_page_template_settings settings config =
  let get_template name settings config =
    (* Retrieve a subtable for given template *)
    let config = get_table name config in
    match config with
    | None -> settings
    | Some config -> begin
      let path_options = get_path_options config in
      let file = get_string "file" config in
      match file with
      | None ->
        let () = Logs.warn @@ fun m -> m "Missing required option \"file\" in [templates.%s], ignoring" name in
        settings
      | Some file ->
        try
          let tmpl_data = Soup.read_file file in
          let templates = (tmpl_data, name, path_options) :: settings.page_templates in
          {settings with page_templates=templates}
        with Sys_error msg ->
          let () = Logs.warn @@ fun m -> m "Could not load the file for [templates.%s]: %s, ignoring" name msg in
          settings
    end
  in
  let tt = get_table Defaults.templates_table config in
  match tt with
  | None -> settings
  | Some tt ->
    let ks = list_config_keys tt in
    List.fold_left (fun s k -> get_template k s tt) settings ks

let valid_settings = [
  "verbose"; "debug"; "strict"; "site_dir"; "build_dir";
  "content_selector"; "doctype"; "index_page"; "index_file";
  "default_template"; "clean_urls"; "page_file_extensions";
  "ignore_extensions"; "default_extension"; "keep_extensions";
  "complete_page_selector"; "generator_mode";
  "plugin_dirs"; "plugin_discovery"
]

let _update_settings settings config =
  let st = get_table Defaults.settings_table config in
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
       content_selector = get_string_default settings.content_selector "content_selector" st;
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

let update_settings settings config =
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
