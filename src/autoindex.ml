open Defaults
open Soupault_common

(* Converts an HTML element tree node to text.
   If [strip_tags] is true, then it extracts all text nodes,
   else it's like innerHTML in JS.
 *)
let string_of_elem ~strip_tags e =
  if strip_tags then Html_utils.get_element_text e
  else begin
    let text = Html_utils.inner_html e in
    match (String.trim text) with
    | "" -> None
    | _ as t -> Some t
  end

(* JSON conversion. *)
let json_of_string_opt s =
  match s with
  | None -> `Null
  | Some s -> `String s

(* Extract index fields from a page using selectors from the index config. *)
let rec get_fields strip_tags fields soup =
  let get_content f elem =
    match f.extract_attribute with
    | None -> string_of_elem ~strip_tags:strip_tags elem
    | Some attr -> begin
      match (Soup.attribute attr elem) with
      | Some _ as a -> a
      | None ->
        if f.fallback_to_content then string_of_elem ~strip_tags:strip_tags elem
        else None
    end
  in
  let get_field f soup =
    if f.select_all then
      `A (Html_utils.select_all f.field_selectors soup |> List.map (fun e -> get_content f e |> json_of_string_opt))
    else
      let (>>=) = Option.bind in
      let e = Html_utils.select_any_of f.field_selectors soup >>= get_content f in
      match e, f.default_field_value with
      | None, None ->
        if f.required_field then soupault_error @@
          Printf.sprintf {|required index field "%s" is missing|} f.field_name
        else `Null
      | None, Some v -> `String v
      | Some e, _ -> `String e
  in
  match fields with
  | [] -> []
  | f :: fs ->
    let field = (f.field_name, get_field f soup) in
    field :: (get_fields strip_tags fs soup)

(* Prepares a complete entry together with built-in meta-fields. *)
let get_entry settings env soup =
  {
    index_entry_url = env.page_url;
    index_entry_page_file = env.page_file;
    index_entry_nav_path = env.nav_path;
    fields = get_fields settings.index_strip_tags settings.index_fields soup
  }

let json_of_entry = Utils.json_of_index_entry

(* Compares entries for sorting, using [Defaults.sort_options] for comparison rules.. *)
let compare_entries settings sort_options l r =
  let (>>=) = Option.bind in
  let (let*) = Option.bind in
  (* We convert all values to strings to make the rest of comparison logic uniform. *)
  let string_of_field j =
    match j with
    | `O _ | `A _ ->
      (* Pathological cases when a field value is an array or an object
         rather than a primitive that has a natural string representation.

         Normally shouldn't happen, just a safeguard to prevent internal logic errors
         and poor handling of values returned by post-index hooks. *)
      let () = Logs.info @@ fun m -> m "Field value is not a primitive: %s" (Ezjsonm.value_to_string j) in
      None
    | _ ->
      (* Everything is fine and it's a primitive value that we can convert to string,
         but there's a pitfall.

         We do not use [Ezjsonm.value_to_string] here because it quotes all values (as of 1.3.0 at least).
         E.g., [`Float 4.0] would become [""4.0""], not ["4.0"].
         That would interfere with date and number parsing at the sorting stage,
         so we use a home-grown string conversion to prevent that.
       *)
      Some (Utils.string_of_json_primitive j)
  in
  let get_sort_key_field entry =
    match sort_options.sort_by with
    | None ->
      (* If [index.sort_by] option is not set, sort entries by their URLs.
         Unlike fields extracted from the page, URL is guaranteed to be present
         and provides a somewhat strange but deterministic default order.
       *)
      Some entry.index_entry_url
    | Some _ ->
      let* sort_by_value = sort_options.sort_by in
      let* field = List.assoc_opt sort_by_value entry.fields in
      string_of_field field
  in
  let compare_values cmp_func l_val r_val =
    match l_val, r_val with
    | None, None ->
      (* Neither is a valid value, resort to lexicographic sort and compare original strings. *)
      compare l r
    | Some _, None -> 1
    | None, Some _ -> -1
    | Some l_val, Some r_val ->
      cmp_func l_val r_val
  in
  let handle_missing_field e v =
    match v with
    | Some _ -> v
    | None ->
      if sort_options.sort_strict then Printf.ksprintf soupault_error
        "Cannot sort entries using sort_by=\"%s\", the following entry does not have that field:\n%s"
        (Option.get sort_options.sort_by)
        (e |> json_of_entry |> Ezjsonm.to_string ~minify:false)
      else v
  in
  let handle_malformed_field type_name orig e v =
    match v with
    | Some _ -> v
    | None ->
      if sort_options.sort_strict then Printf.ksprintf soupault_error
        "Cannot sort entries using sort_by=\"%s\": value \"%s\" could not be parsed as %s. The offending entry is:\n%s"
        (Option.get sort_options.sort_by) (Option.value ~default:"null" orig) type_name
        (e |> json_of_entry |> Ezjsonm.to_string ~minify:false)
      else v
  in
  let l_key = get_sort_key_field l |> handle_missing_field l in
  let r_key = get_sort_key_field r |> handle_missing_field r in
  let result =
    match sort_options.sort_type with
    | Calendar ->
      (* Compare entry dates according to these rules:
         1. Entries without known dates are equal
         2. Entries with a known date are newer than those without
         3. Of entries with known dates, ones with later dates are newer (who could guess!)
       *)
      let l_date = l_key >>= Utils.parse_date settings.index_date_input_formats |> handle_malformed_field "a date" l_key l in
      let r_date = r_key >>= Utils.parse_date settings.index_date_input_formats |> handle_malformed_field "a date" r_key r in
      compare_values ODate.Unix.compare l_date r_date
    | Numeric ->
      (* Numeric comparison needs to always be prepared to handle floats even if they are integers in the page.
         Numeric fields returned by Lua plugins are always floats because Lua doesn't have an integer/float distinction. *)
      let l_num = l_key >>= (fun s -> float_of_string_opt s) |> handle_malformed_field "a number" l_key l in
      let r_num = r_key >>= (fun s -> float_of_string_opt s) |> handle_malformed_field "a number" r_key r in
      compare_values compare l_num r_num
    | Lexicographic ->
      (* For lexicographic sort we use simple comparison from the standard library,
         for now at least.
       *)
      compare_values compare l_key r_key
  in
  if sort_options.sort_descending then (~- result) else result

let sort_entries settings sort_options es =
  try Ok (List.sort (compare_entries settings sort_options) es)
  with Soupault_error msg -> Error msg

let json_of_entries = Utils.json_of_index_entries

let json_string_of_entries ?(minify=false) es =
  json_of_entries es |> Ezjsonm.to_string ~minify:minify

let jingoo_model_of_entry e =
  let j = json_of_entry e in
  match j with
  | `O js -> List.map (fun (k, v) -> k, Template.jingoo_of_json v) js
  | _ -> Printf.ksprintf internal_error
    "json_of_entry returned something else than an object, which must not happen!\nThe value was:\n%s"
    (Ezjsonm.value_to_string j)

(* Renders an index using a Jingoo template.

   This rendering had two modes: item template and whole-index template.

   The item template mode is a legacy of the original implementation that uses Mustache templates.
   Since Mustache templates are logicless, soupault had to iterate through all entries itself.
   That mode is triggered by the [index_item_template] option.

   Migration to Jingoo templates enabled users to write their own rendering loops and supply complete templates
   through [index_template, but the original [index_item_template] option remains
   for compatibility and because for some users it may be all they need.
 *)
let render_index ?(item_template=true) soupault_config view template settings soup entries =
  let () = Logs.info @@ fun m -> m "Generating section index" in
  try
    let () =
      (* Debug output *)
      if settings.debug then
      Logs.debug @@ fun m -> m "Index data (pretty-printed): %s" (json_string_of_entries ~minify:false entries)
    in
    let entries =
      if item_template then
        List.map (fun e -> jingoo_model_of_entry e |> Template.render template |> Soup.parse) entries
      else
        let env = [
          "soupault_config", Template.jingoo_of_toml soupault_config;
          "entries", Template.jingoo_of_json (json_of_entries entries)
        ]
        in
        [Template.render template env |> Soup.parse]
    in
    let () = List.iter (Html_utils.insert_element view.index_action soup) entries in
    Ok ()
  with
  | Failure err ->
    (* Jingoo raises Failure on rendering errors, though it's not a frequent occurence. *)
    let msg = Printf.sprintf "Index template rendering failed: %s" err in
    if settings.ignore_template_errors
    then let () = Logs.warn @@ fun m -> m "%s" msg in Ok ()
    else Error msg
  | _ ->
    (* Just in case something else happens *)
    Error ("Index template rendering failed for an undeterminable reason")

(* Renders index with help from an external executable. *)
let run_index_processor view cmd ic index =
  (* Do not pretty-print JSON, the parser on the other end doesn't care. *)
  let json = json_string_of_entries ~minify:true index in
  let () = Logs.info @@ fun m -> m "Calling index processor %s" cmd in
  let output = (Process_utils.get_program_output ~input:(Some json) cmd) in
  begin
    match output with
    | Error _ as e -> e
    | Ok output -> Ok (Html_utils.insert_element view.index_action ic (Soup.parse output))
  end

let view_includes_page settings page_file view entry =
  if (Path_options.is_default view.index_view_path_options) then
    (* If the user hasn't configured the view to specifically include
       or exclude any pages, assume they want an index of the current section
       and its subsections -- more or less like it worked before 2.0.0 *)
    let include_subsections = view.index_view_path_options.include_subsections in
    let entry_file =
      (* This is essentially a reverse check: if the current file the index page of a certain section?
         Thus we need to apply the "hand-made clean URL" fixup to the page we are checking against
         (the index_entry_page_file).
       *)
      if Path_options.is_handmade_clean_url settings entry.index_entry_page_file
      then FilePath.dirname entry.index_entry_page_file
      else entry.index_entry_page_file
    in
    Path_options.section_matches ~include_subsections:include_subsections settings "" page_file (FilePath.dirname entry_file)
  else
    Path_options.page_included settings view.index_view_path_options settings.site_dir entry.index_entry_page_file

(* Sort options "inheritance" *)
let get_sort_options settings view =
  let redefine l r =
    if l <> r then r else l
  in
  match view.index_view_sort_options with
  | None ->
    (* If there are no configured sort options in a view,
       use global options from the [index] table. *)
       settings.index_sort_options
  | Some view_sort_options ->
    (* Else use options from the view if they are different from global options. *)
    let l = settings.index_sort_options in
    let r = view_sort_options in
    {
      sort_by = redefine l.sort_by r.sort_by;
      sort_type = redefine l.sort_type r.sort_type;
      sort_descending = redefine l.sort_descending r.sort_descending;
      sort_strict = redefine l.sort_strict r.sort_strict;
    }
  
let insert_index env soupault_config soup view =
 let index_container = Soup.select_one view.index_selector soup in
  match index_container with
  | None ->
    let () = Logs.debug @@ fun m -> m {|Page "%s" doesn't have an element matching selector "%s", ignoring index view "%s"|}
      env.page_file view.index_selector view.index_view_name
    in Ok []
  | Some ic ->
    begin
      let () = Logs.info @@ fun m -> m {|Rendering index view "%s" on page %s|} view.index_view_name env.page_file in
      let (let*) = Result.bind in
      let index = List.filter (view_includes_page env.settings env.page_file view) env.site_index in
      let* index = sort_entries env.settings (get_sort_options env.settings view) index in
      match view.index_processor with
      | Defaults.IndexItemTemplate tmpl ->
        let* () = render_index soupault_config view tmpl env.settings ic index in Ok []
      | Defaults.IndexTemplate tmpl ->
        let* () = render_index ~item_template:false soupault_config view tmpl env.settings ic index in Ok []
      | Defaults.ExternalIndexer cmd ->
        let* () = run_index_processor view cmd ic index in Ok []
      | Defaults.LuaIndexer (file_name, lua_code) ->
        let index_view_config = Otoml.find soupault_config Otoml.get_table ["index"; "views"; view.index_view_name] |> Otoml.table in
        (* Give the Lua index processor a filtered index view rather than the original full version. *)
        let env = {env with site_index=index} in
        Hooks.run_lua_index_processor soupault_config index_view_config view.index_view_name file_name lua_code env soup
    end

let insert_indices env soupault_config soup =
  let (let*) = Result.bind in
  let insert_index_get_pages env soupault_config soup acc view =
    let* pages = insert_index env soupault_config soup view in
    Ok (List.append pages acc)
  in
  Utils.fold_left_result ~ignore_errors:(not env.settings.strict)
    (insert_index_get_pages env soupault_config soup) [] env.settings.index_views

let index_extraction_should_run settings page_file =
  (* If this option is true, this is a second pass and we don't need to extract anything
     since the complete website metadata is already available from the first pass. *)
  if settings.no_index_extraction then false else
  (* If indexing is disabled in the config, it definitely should not run. *)
  if not settings.index then false else
  (* ...as well as if indexing is disabled by build profile settings. *)
  if not (Utils.build_profile_matches settings.index_profile settings.build_profiles) then false else
  (* *)
  if (Path_options.is_handmade_clean_url settings page_file) then true else
  (* Metadata is not extracted from section index, unless forced by forced_indexing_path_regex.
     The only valid reason to extract metadata from an section/index.html page is to account for
     hand-made "clean URLs", otherwise they usually don't contain any content other than pointers
     to other pages.
   *)
  let page_name = FilePath.basename page_file |> FilePath.chop_extension in
  if (page_name = settings.index_page) then false else
  (* A normal, non-index page may still be excluded from indexing. *)
  if not (Path_options.page_included settings settings.index_path_options settings.site_dir page_file) then
    let () = Logs.debug @@ fun m -> m "Page %s excluded from indexing by page/section/regex options" page_file in
    false
  else true
