open Defaults

let bind = CCResult.(>>=)

let string_of_elem strip_tags e =
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

let rec get_custom_fields strip_tags fields soup =
  let get_content f elem =
    match f.extract_attribute with
    | None -> string_of_elem strip_tags elem
    | Some attr -> begin
      match (Soup.attribute attr elem) with
      | Some _ as a -> a
      | None ->
        if f.fallback_to_content then string_of_elem strip_tags elem
        else None
    end
  in
  let get_field f soup =
    if f.select_all then
      `A (Html_utils.select_all f.field_selectors soup |> List.map (fun e -> get_content f e |> json_of_string_opt))
    else
      let (>>=) = Stdlib.Option.bind in
      let e = Html_utils.select_any_of f.field_selectors soup >>= get_content f in
      match e, f.default_field_value with
      | None, None -> `Null
      | None, Some v -> `String v
      | Some e, _ -> `String e
  in
  match fields with
  | [] -> []
  | f :: fs ->
    let field = (f.field_name, get_field f soup) in
    field :: (get_custom_fields strip_tags fs soup)

let get_entry settings env soup =
  {
    index_entry_url = env.page_url;
    index_entry_page_file = env.page_file;
    index_entry_nav_path = env.nav_path;
    fields = get_custom_fields settings.index_strip_tags settings.index_fields soup
  }

(** Compares entries by their dates according to these rules:
    1. Entries without known dates are equal
    2. Entries with a known date are newer than those without
    3. Of entries with known dates, ones with later dates are newer (who could guess!)
  *)
let compare_entries settings l r =
  let (>>=) = Stdlib.Option.bind in
  let string_of_field j =
    try Some (Toml_utils.string ~strict:false j)
    with Toml_utils.Type_error _ -> None
  in
  let get_date entry =
    settings.index_sort_by >>= (fun f -> List.assoc_opt f entry.fields) >>=
    string_of_field >>= Utils.parse_date settings.index_date_input_formats
  in
  let compare_dates l_date r_date =
    match l_date, r_date with
    | None, None ->
      (* Neither is a valid date, resort to lexicographic sort *)
      compare l r
    | Some _, None -> 1
    | None, Some _ -> -1
    | Some l_date, Some r_date ->
      CalendarLib.Date.compare l_date r_date
  in
  let l_date = get_date l in
  let r_date = get_date r in
  let result = compare_dates l_date r_date in
  if settings.index_sort_descending then (~- result) else result

let json_of_entry e =
  let fields = [
    ("url", `String e.index_entry_url);
    ("page_file", `String e.index_entry_page_file);
    ("nav_path", `A (List.map (fun x -> `String x) e.index_entry_nav_path))
  ] in
  let fields = List.append fields e.fields in
  `O fields

let sort_entries settings es = List.sort (compare_entries settings) es

let json_of_entries es =
  `A (List.map json_of_entry es)

let json_string_of_entries ?(minify=false) es =
  json_of_entries es |> Ezjsonm.to_string ~minify:minify

let jingoo_model_of_entry e =
  let j = json_of_entry e in
  match j with
  | `O js -> List.map (fun (k, v) -> k, Template.jingoo_of_json v) js
  | _ -> failwith "json_of_entry returned something else than an object, which must not happen"

(** Renders an index using built-in Mustache templates *)
let render_index ?(item_template=true) template settings soup entries =
  let () = Logs.info @@ fun m -> m "Generating section index" in
  try
    let () =
      (* Debug output *)
      if settings.debug then
      Logs.debug @@ fun m -> m "Index data (pretty-printed): %s" (json_string_of_entries ~minify:false entries)
    in
    let entries = List.sort (compare_entries settings) entries in
    let entries =
      if item_template then List.map (fun e -> jingoo_model_of_entry e |> Template.render template |> Soup.parse) entries
      else [Template.render template @@ ["entries", Template.jingoo_of_json (json_of_entries entries)] |> Soup.parse]
    in
    let () = List.iter (Soup.append_child soup) entries in
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

let run_index_processor cmd ic index =
  (* Minification is intentional, newline is used as end of input *)
  let json = json_string_of_entries ~minify:true index in
  let () = Logs.info @@ fun m -> m "Calling index processor %s" cmd in
  let output = Utils.get_program_output ~input:(Some json) cmd [| |] in
  begin
    match output with
    | Error _ as e -> e
    | Ok output -> Ok (Soup.append_child ic (Soup.parse output))
  end

let view_includes_page settings page_file view entry =
  if (Path_options.is_default view.index_view_path_options) then
    (* If the user hasn't configured the view to specifically include
       or exclude any pages, assume they want an index of the current section
       and its subsections -- more or less like it worked before 2.0.0 *)
    let include_subsections = view.index_view_path_options.include_subsections in
    Path_options.section_matches ~include_subsections:include_subsections "" page_file (FilePath.dirname entry.index_entry_page_file)
  else
    Path_options.page_included view.index_view_path_options settings.site_dir entry.index_entry_page_file

let insert_index settings page_file soup index view =
  let index_container = Soup.select_one view.index_selector soup in
  match index_container with
  | None ->
    let () = Logs.warn @@ fun m -> m "Page doesn't have an element matching selector \"%s\", ignoring index view \"%s\""
      view.index_selector view.index_view_name
    in Ok ()
  | Some ic ->
    begin
      let index = List.filter (view_includes_page settings page_file view) index in
      match view.index_processor with
      | Defaults.IndexItemTemplate tmpl -> render_index tmpl settings ic index
      | Defaults.IndexTemplate tmpl -> render_index ~item_template:false tmpl settings ic index
      | Defaults.ExternalIndexer cmd -> run_index_processor cmd ic index
    end

let insert_indices settings page_file soup index =
  Utils.iter ~ignore_errors:(not settings.strict) (insert_index settings page_file soup index) settings.index_views

let index_extraction_should_run settings page_file =
  if not (Utils.profile_matches settings.index_profile settings.build_profile) then
    let () = Logs.debug @@ fun m -> m "Index extraction is disabled by build profile options" in false
  else begin
    if Path_options.page_included settings.index_path_options settings.site_dir page_file then true
    else
      let () = Logs.debug @@ fun m -> m "Page %s excluded from indexing by page/section/regex options" page_file in
      false
  end
