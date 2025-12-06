module OH = Otoml.Helpers

open Common

open Otoml

let check_selector f opt_name s =
  let res = Html_utils.check_selector opt_name s in
  match res with
  | Ok () -> ()
  | Error msg -> Printf.ksprintf f "%s" msg

let check_selectors f opt_name s =
  let res = Html_utils.check_selectors opt_name s in
  match res with
  | Ok () -> ()
  | Error msg -> Printf.ksprintf f "%s" msg

let required_option res =
  match res with
  | Ok v -> v
  | Error msg -> config_error msg

(* A helper for unwrapping result values. *)
let of_result base_msg value =
  match value with
  | Ok v -> v
  | Error msg -> config_error @@ Printf.sprintf "%s: %s" base_msg msg

(* For internal use, to avoid re-raising Config_error *)
exception Index_view_error of string
let index_view_error err = raise (Index_view_error err)

let sort_type_from_string s =
  match s with
  | "calendar" -> Calendar
  | "numeric" -> Numeric
  | "lexicographic" -> Lexicographic
  | _ -> Printf.ksprintf config_error {|"%s" is not a valid value for the "sort_type" option.\
    Choose either "calendar", "numeric" or "lexicographic"|} s

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
  "section"; "exclude_section"; "include_subsections";
  "profile";
  "after";
  "disabled"
]

let bad_option_msg opt ident suggestion =
  let suggestion_msg =
    (match suggestion with
    | None -> ""
    | Some s -> Printf.sprintf {|Did you mean "%s"?|} s)
  in Printf.sprintf {|Option "%s" is not valid for %s. %s|} opt ident suggestion_msg

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
    None
  else
  try
    let conf = Otoml.Parser.from_file path in
    Some conf
  with
  | Sys_error err -> Printf.ksprintf soupault_error "Could not read config file: %s" err
  | Otoml.Parse_error (pos, msg) ->
    Printf.ksprintf soupault_error "Could not parse config file %s: %s"
      path (Otoml.Parser.format_parse_error pos msg)
  | Otoml.Duplicate_key msg ->
    (* As of OTOML 1.0.4, parsing a syntactically correct config
       that contains duplicate table names raises Duplicate_key rather than Parse_error,
       so we need to handle it separately.
     *)
    Printf.ksprintf soupault_error "Config file %s is malformed: %s" path msg

(* Convenience accessor wrappers *)

let find_table path config = TomlTable (find config get_table path)
let find_table_opt path config = find_opt config get_table path |> Option.map (fun x -> TomlTable x)
let find_table_result err path config = find_opt config get_table path |> Option.to_result ~none:err

let find_result accessor config path =
  try Ok (Otoml.find config accessor path)
  with
  | Otoml.Key_error _ ->
    Error (Printf.sprintf {|missing required option "%s"|} (Otoml.string_of_path path))
  | Otoml.Type_error msg ->
    Error (Printf.sprintf {|wrong type for option "%s": %s|} (Otoml.string_of_path path) msg)

let find accessor config path =
  let res = find_result accessor config path in
  match res with
  | Ok v -> v
  | Error msg -> config_error msg

let find_string ?(strict=false) config path =
  find (Otoml.get_string ~strict:strict) config path

let find_string_or ?(strict=false) ~default:default config path =
  OH.find_string_opt ~strict:strict config path |> Option.value ~default:default

let find_strings_or ~default:default config path =
  try OH.find_strings config path 
  with Key_error _ -> default

let find_strings ?(strict=false) config path =
  let get_strings = Otoml.get_array ~strict:false (Otoml.get_string ~strict:strict) in
  find get_strings config path

let find_bool_or ?(strict=false) ~default:default config path =
  OH.find_boolean_opt ~strict:strict config path |> Option.value ~default:default

let find_integer_or ?(strict=false) ~default:default config path =
  OH.find_integer_opt ~strict:strict config path |> Option.value ~default:default

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

(* Get the mapping of file extensions to page preprocessor commands.
   Page preprocessors arguments are simply appended to the command
   since they aren't expected to need anything but an input file path.
   That allows us to store them simply as string.
 *)
let _get_page_preprocessors config =
  let t = find_table_opt ["preprocessors"] config in
  match t with
  | None -> []
  | Some t ->
    let t = get_table t in
    CCList.Assoc.map_values get_string t

(* Get the mapping of file extensions to asset processors.
   Since asset processors are expected to both process a file and write
   the result to a target location, it's a bit more involved than with page preprocessors.

   That's why we store them as Jingoo templates that can take input and output files
   and precompile those templates for speed.
 *)
let _get_asset_processors config =
  let get_processor extension toml_value =
    try
      let template_string = get_string toml_value in
      Template.of_string template_string
    with Soupault_error msg ->
      (* XXX: maybe Template.render should use a special exception rather than Soupault_error.
         At the config parsing stage, Soupault_error is not handled,
         which is why we have to catch it and re-raise it as Config_error instead,
       *)
      let () = Logs.err @@ fun m -> m {|Could not compile template from asset_processors.%s|} extension in
      config_error msg
  in
  let t = find_table_opt ["asset_processors"] config in
  match t with
  | None -> []
  | Some t ->
    let t = get_table t in
    Utils.assoc_map_key_values get_processor t

let get_index_queries index_table =
  let get_query k it =
    let selectors = find_strings_or ~default:[] it [k; "selector"] in
    let default_value = OH.find_string_opt it [k; "default"] in
    let extract_attribute = OH.find_string_opt it [k; "extract_attribute"] in
    let content_fallback = find_bool_or ~default:false it [k; "fallback_to_content"] in
    let strip_tags = find_bool_or ~default:false it [k; "strip_tags"] in
    let select_all = find_bool_or ~default:false it [k; "select_all"] in
    let required = find_bool_or ~default:false it [k; "required"] in
    let () =
      if (Option.is_some default_value) && select_all then
      Logs.warn @@ fun m -> m {|"default" is ignored when "select_all" is true|}
    in
    (match selectors with
    | [] -> config_error {|"selector" option is required and must be a string or a list of strings|}
    | _ ->
      let () = check_selectors index_view_error "selector" selectors in
      {
        field_name = k;
        field_selectors = selectors;
        select_all = select_all;
        default_field_value = default_value;
        extract_attribute = extract_attribute;
        fallback_to_content = content_fallback;
        strip_field_tags = strip_tags;
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
          Printf.ksprintf config_error {|Malformed config for index field "%s": %s|} k err
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
  "extract_after_widgets"; "strip_tags";
  "force_indexing_path_regex"; "leaf_file";
  "profile"
] @ valid_path_options

(* Retrieve sort options.
   The tricky part is that any setting may match its default for two distinct reasons:
   1. It's not set at all and that's why the default is used.
   2. It's explicitly set in the config but its value happens to be the same as the default.

   That is why we have to check if any of those options exist in the config or not.
   If none are set explicitly, then we use the global sorting settings.
 *)
let get_sort_options toml =
  let sort_by = OH.find_string_opt toml ["sort_by"] in
  let sort_type = OH.find_string_opt toml ["sort_type"] in
  let sort_descending = OH.find_boolean_opt toml ["sort_descending"] in
  let sort_strict = OH.find_boolean_opt toml ["strict_sort"] in
  match sort_by, sort_type, sort_descending, sort_strict with
  | None, None, None, None ->
    (* XXX: If the number of options grows, make a helper that will check for presence of any options
            from a given list to check if something is completely unconfigured. *)
    None
  | _ -> Some {
      sort_by = sort_by;
      sort_type = Option.value ~default:"calendar" sort_type |> sort_type_from_string;
      sort_descending = Option.value ~default:true sort_descending;
      sort_strict = Option.value ~default:false sort_strict
    }

let _get_index_view st view_name =
  let _get_template ?(item_template=true) tmpl =
    begin
      try
        let t = Template.of_string tmpl in
        if item_template then IndexItemTemplate t
        else IndexTemplate t
      with _ ->
        (* Jingoo does not provide meaningful parse error reporting as of 1.4.4. *)
        index_view_error (Printf.sprintf "Failed to parse template:\n%s" tmpl)
    end
  in
  let _get_index_processor name st =
    let get_lua_index_processor st =
      (* Utils.load_plugin_code assumes that if neither "file" nor "lua_source" are set,
         it's an error, because in plugins and hooks that's the case.
         Index views is a special case though, file/lua_source is just one of the options they can use.
         That's why this wrapper is needed, to distinguish cases when those options are absent and when they are present
         but their values are incorrect.
       *) 
      let lua_source = OH.find_string_opt ~strict:true st ["lua_source"] in
      let lua_file = OH.find_string_opt ~strict:true st ["file"] in
      match lua_source, lua_file with
      | None, None -> None
      | _, _ ->
        let res = Utils.load_plugin_code st (Printf.sprintf {|<inline Lua code from index view "%s">|} name)
          (Printf.sprintf {|Lua index processor for view "%s"|} name)
        in Some res
    in
    let item_template = OH.find_string_opt ~strict:true st ["index_item_template"] in
    let index_template = OH.find_string_opt ~strict:true st ["index_template"] in
    let script = OH.find_string_opt ~strict:true st ["index_processor"] in
    let lua_processor = get_lua_index_processor	st in
    match item_template, index_template, script, lua_processor with
    | _, _, Some _, Some _ ->
      index_view_error {|"index_processor" and "file"/"lua_source" are mutually exclusive, please pick only one|}
    | _, _, _, Some (file_name, lua_code) -> LuaIndexer (file_name, lua_code)
    | Some item_template, None, None, None -> _get_template item_template
    | None, Some index_template, None, None -> _get_template ~item_template:false index_template
    | None, None, Some script, None -> ExternalIndexer script
    | None, None, None, None ->
      Printf.ksprintf index_view_error "index view does not specify any index rendering options. \
        Please specify one of: index_item_template, index_template, index_processor, \
          file (for an external Lua script), or lua_source"
    | _ -> index_view_error "options index_item_template, index_template, and index_processor \
       are mutually exclusive, please pick only one"
  in
  let selector = OH.find_string st ["index_selector"] in
  let () = check_selector index_view_error "index_selector" selector in
  let action = OH.find_string_opt st ["action"] in
  let index_processor = _get_index_processor view_name st in
  let max_items = OH.find_integer_opt ~strict:true st ["max_items"] in
  {
    index_view_name = view_name;
    index_selector = selector;
    index_action = action;
    index_processor = index_processor;
    index_view_path_options = (get_path_options st);
    index_view_sort_options = get_sort_options st;
    max_items = max_items;
  }

let _get_index_views index_table =
  let get_view k views =
    let vt = find_table [k] views in
    (* No option validation, since index views allow passing arbitrary options to index processor plugins. *)
    _get_index_view vt k
  in
  let rec get_views ks views acc =
    match ks with
    | [] -> acc
    | k :: ks -> begin
      try
        let v = get_view k views in
        get_views ks views (v :: acc)
      with Index_view_error e | Key_error e | Type_error e ->
        Printf.ksprintf config_error {|Misconfigured index view "%s": %s|} k e
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
  let st = find_table_opt ["index"] config in
  match st with
  | None -> settings
  | Some st ->
    let () = check_options valid_index_options st {|table "index"|} in
    let date_formats = find_strings_or ~default:settings.index_date_input_formats st ["date_formats"] in
    let () =
      let index = OH.find_boolean_opt ~strict:false st ["index"] in
      begin match index with
      | Some _ ->
        Logs.warn @@ fun m -> m "index.index option is deprecated and has no effect"
      | None -> ()
      end
    in
    {settings with
       dump_index_json = OH.find_string_opt st ["dump_json"];
       index_extract_after_widgets = find_strings_or ~default:[] st ["extract_after_widgets"];
       index_fields = get_index_queries st;
       index_strip_tags = find_bool_or ~default:settings.index_strip_tags st ["strip_tags"];
       index_views = _get_index_views st;
       index_profile = OH.find_string_opt st ["profile"];
       index_path_options = get_path_options st;
       index_sort_options = get_sort_options st |> Option.value ~default:Common.default_sort_options;
       index_date_input_formats = date_formats;
       index_force = find_strings_or ~default:[] st ["force_indexing_path_regex"];
       index_leaf_file = OH.find_string_opt st ["leaf_file"];
    }

let update_page_template_settings settings config =
  let get_template name settings config =
    (* Retrieve the config subtable for the template by its name,
       like [templates.main]
     *)
    let config = find_table_opt [name] config in
    match config with
    | None -> settings
    | Some config -> begin
      let path_options = get_path_options config in
      let file = OH.find_string_opt config ["file"] in
      let content_selector = OH.find_string_opt config ["content_selector"] in
      let () =
        match content_selector with
        | None -> ()
        | Some cs ->
          check_selector config_error
            (Printf.sprintf "templates.%s.content_selector" name) cs
      in
      let content_action = OH.find_string_opt config ["content_action"] in
      match file with
      | None -> Printf.ksprintf config_error {|Missing required option "file" in [templates.%s]|} name
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
          Printf.ksprintf config_error "Could not load the file for [templates.%s]: %s" name msg
    end
  in
  let tt = find_table_opt ["templates"] config in
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
  "clean_urls"; "clean_url_trailing_slash";
  "page_file_extensions"; "ignore_extensions"; "default_extension"; "keep_extensions";
  "markdown_extensions";
  "markdown_strict_commonmark";
  "markdown_smart_punctuation";
  "markdown_smart_quotes"; "markdown_smart_apostrophe";
  "markdown_smart_dashes"; "markdown_smart_ellipsis";
  "ignore_path_regexes"; "ignore_directories";
  "complete_page_selector"; "generator_mode";
  "plugin_dirs"; "plugin_discovery";
  "force"; "caching"; "cache_dir";
  "page_character_encoding";
  "pretty_print_html";
  "soupault_version";
]

let check_deprecated_settings settings_table =
  let strict = OH.find_boolean_opt ~strict:false settings_table ["strict"] in
  match strict with
  | Some _ ->
    Logs.warn @@ fun m -> m "settings.strict option is deprecated and has no effect"
  | None -> ()

let _update_settings settings config =
  let st = find_table_opt ["settings"] config in
  match st with
  | None ->
     let () = Logs.warn @@ fun m -> m "Could not find the [settings] section in the config, using defaults" in
     settings
  | Some st ->
    let () = check_options valid_settings st {|table "settings"|} in
    let () = check_deprecated_settings st in
    let settings = update_page_template_settings settings config in
    let default_content_selector = find_string_or ~default:settings.default_content_selector st ["default_content_selector"] in
    let () = check_selector config_error "settings.default_content_selector" default_content_selector in
    let complete_page_selector = find_string_or ~default:settings.complete_page_selector st ["complete_page_selector"] in
    let () = check_selector config_error "settings.complete_page_selector" complete_page_selector in
    let settings = {settings with
       verbose = find_bool_or ~default:settings.verbose st ["verbose"];
       debug = find_bool_or ~default:settings.debug st ["debug"];
       site_dir = find_string_or ~default:settings.site_dir st ["site_dir"] |> String.trim;
       build_dir = find_string_or ~default:settings.build_dir st ["build_dir"] |> String.trim |> File_path.normalize_path;
       default_content_selector = default_content_selector;
       doctype = find_string_or ~default:settings.doctype st ["doctype"];
       keep_doctype = find_bool_or ~default:settings.keep_doctype st ["keep_doctype"];
       index_page = find_string_or ~default:settings.index_page st ["index_page"];
       index_file = find_string_or ~default:settings.index_file st ["index_file"];
       default_template = find_string_or ~default:settings.default_template st ["default_template_file"];
       default_content_action = find_string_or ~default:settings.default_content_action st ["default_content_action"];
       clean_urls = find_bool_or ~default:settings.clean_urls st ["clean_urls"];
       clean_url_trailing_slash = find_bool_or ~default:settings.clean_url_trailing_slash st ["clean_url_trailing_slash"];
       page_extensions = find_strings_or ~default:settings.page_extensions st ["page_file_extensions"];
       markdown_extensions = find_strings_or ~default:settings.markdown_extensions st ["markdown_extensions"];
       markdown_strict_commonmark = find_bool_or ~default:settings.markdown_strict_commonmark st ["markdown_strict_commonmark"];
       markdown_smart_punctuation = find_bool_or ~default:settings.markdown_smart_punctuation st ["markdown_smart_punctuation"];
       markdown_smart_dashes = find_bool_or ~default:settings.markdown_smart_dashes st ["markdown_smart_dashes"];
       markdown_smart_quotes = find_bool_or ~default:settings.markdown_smart_quotes st ["markdown_smart_quotes"];
       markdown_smart_apostrophe = find_bool_or ~default:settings.markdown_smart_apostrophe st ["markdown_smart_apostrophe"];
       markdown_smart_ellipsis = find_bool_or ~default:settings.markdown_smart_ellipsis st ["markdown_smart_ellipsis"];
       ignore_extensions = find_strings_or ~default:[] st ["ignore_extensions"];
       ignore_path_regexes = find_strings_or ~default:[] st ["ignore_path_regexes"];
       ignore_directories = find_strings_or ~default:[] st ["ignore_directories"];
       keep_extensions = find_strings_or ~default:settings.keep_extensions st ["keep_extensions"];
       default_extension = find_string_or ~default:settings.default_extension st ["default_extension"];
       complete_page_selector = complete_page_selector;
       generator_mode = find_bool_or ~default:settings.generator_mode st ["generator_mode"];

       plugin_dirs = find_strings_or ~default:settings.plugin_dirs st ["plugin_dirs"];
       plugin_discovery = find_bool_or ~default:settings.plugin_discovery st ["plugin_discovery"];

       caching = find_bool_or ~default:settings.caching st ["caching"];
       cache_dir = find_string_or ~default:settings.cache_dir st ["cache_dir"];

       force = find_bool_or ~default:settings.force st ["force"];

       page_character_encoding =
         find_string_or ~default:"utf-8" st ["page_character_encoding"] |>
         Utils.encoding_of_string |>
         of_result "Incorrect value for page_character_endcoding:";

       pretty_print_html = find_bool_or ~default:settings.pretty_print_html st ["pretty_print_html"];

       soupault_version = OH.find_string_opt st ["soupault_version"];

       page_preprocessors = _get_page_preprocessors config;
       asset_processors = _get_asset_processors config;
    }
    in
    (* Update the list of extensions that are considered pages rather than assets.
       It should include:
         1. Extensions explicitly configured in settings.page_file_extensions
         2. Extensions from settings.markdown_extensions
         3. All extensions that are associated with preprocessors
     *)
    let page_preprocessor_extensions = List.map (fun (k, _) -> k) settings.page_preprocessors in
    let page_extensions =
      settings.page_extensions @ settings.markdown_extensions @ page_preprocessor_extensions |>
      CCList.uniq ~eq:(=)
    in
    {settings with page_extensions=page_extensions}

let valid_tables = [
  "settings";
  "templates";
  "index";
  "plugins"; "widgets";
  "preprocessors"; "asset_processors";
  "hooks";
  "custom_options"
]

let check_subsections ?(parent_path=[]) config valid_tables table_name =
  let bad_section_msg tbl _ suggestion =
    let suggestion_msg =
      (match suggestion with
      | None -> ""
      | Some s -> Printf.sprintf {|Did you mean "%s"?|} s)
    in
    let path = List.append parent_path [tbl] |> Otoml.string_of_path in
    Printf.sprintf "[%s] is not a valid config section. %s" path suggestion_msg |> String.trim
  in
  check_options ~fmt:bad_section_msg valid_tables config (Printf.sprintf {|table "%s"|} table_name)

let update_settings_unsafe settings config =
 match config with
  | None -> settings
  | Some config ->
    let () = check_subsections config valid_tables "settings" in
    let settings = _update_settings settings config in
    _get_index_settings settings config

let update_settings settings config =
  try update_settings_unsafe settings config
  with
  | Config_error e -> soupault_error e
  | Otoml.Type_error e ->
    Printf.ksprintf soupault_error "Incorrect config option value: %s" e

let inject_option path new_value config =
  let value = find_opt config (fun x -> x) path in
  match value with
  | Some orig_value ->
    if new_value = orig_value then config
    else update config path (Some new_value)
  | None -> update config path (Some new_value)

let inject_options settings config =
  let inject_default_settings settings config =
      inject_option ["settings"; "verbose"] (boolean settings.verbose) config |>
      inject_option ["settings"; "debug"] (boolean settings.debug) |>
      inject_option ["settings"; "force"] (boolean settings.force) |>
      inject_option ["settings"; "doctype"] (string settings.doctype) |>
      inject_option ["settings"; "keep_doctype"] (boolean settings.keep_doctype) |>
      inject_option ["settings"; "build_dir"] (string settings.build_dir) |>
      inject_option ["settings"; "site_dir"] (string settings.site_dir) |>
      inject_option ["settings"; "index_page"] (string settings.index_page) |>
      inject_option ["settings"; "index_file"] (string settings.index_file) |>
      inject_option ["settings"; "default_template"] (string settings.default_template) |>
      inject_option ["settings"; "default_content_action"] (string settings.default_content_action) |>
      inject_option ["settings"; "default_content_selector"] (string settings.default_content_selector) |>
      inject_option ["settings"; "clean_urls"] (boolean settings.clean_urls) |>
      inject_option ["settings"; "page_file_extensions"] (array @@ List.map string settings.page_extensions) |>
      inject_option ["settings"; "markdown_extensions"] (array @@ List.map string settings.markdown_extensions) |>
      inject_option ["settings"; "ignore_extensions"] (array []) |>
      inject_option ["settings"; "ignore_path_regexes"] (array []) |>
      inject_option ["settings"; "ignore_directories"] (array []) |>
      inject_option ["settings"; "default_extension"] (string settings.default_extension) |>
      inject_option ["settings"; "keep_extensions"] (array @@ List.map string settings.keep_extensions) |>
      inject_option ["settings"; "complete_page_selector"] (string settings.complete_page_selector) |>
      inject_option ["settings"; "generator_mode"] (boolean settings.generator_mode) |>
      inject_option ["settings"; "pretty_print_html"] (boolean settings.pretty_print_html) |>
      inject_option ["settings"; "plugin_discovery"] (boolean settings.plugin_discovery) |>
      inject_option ["settings"; "plugin_dirs"] (array @@ List.map string settings.plugin_dirs) |>
      inject_option ["settings"; "caching"] (boolean settings.caching) |>
      inject_option ["settings"; "cache_dir"] (string settings.cache_dir) |>
      inject_option ["settings"; "page_character_encoding"] (settings.page_character_encoding |> Utils.string_of_encoding |> string)
  in
  let inject_default_index_settings settings config =
      inject_option ["index"; "extract_after_widgets"] (array []) config |>
      inject_option ["index"; "strip_tags"] (boolean settings.index_strip_tags) |>
      inject_option ["index"; "date_formats"] (array @@ List.map string settings.index_date_input_formats) |>
      inject_option ["index"; "force_indexing_path_regex"] (array [])
  in
  let res = inject_default_settings settings config |> inject_default_index_settings settings in
  res
