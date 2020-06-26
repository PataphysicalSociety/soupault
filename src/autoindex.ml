open Defaults

let bind = CCResult.(>>=)

type 'a index_entry = {
  url: string;
  page_file: string;
  nav_path: string list;
  title: string option;
  excerpt: string option;
  date: string option;
  author: string option;
  custom_fields : (string * Ezjsonm.value) list;
}

let string_of_elem strip_tags e =
  if strip_tags then Utils.get_element_text e
  else begin
    let text = Utils.inner_html e in
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
    | Some attr -> Soup.attribute attr elem
  in
  let get_field f soup =
    if f.select_all then
      `A (Utils.select_all f.field_selectors soup |> List.map (fun e -> get_content f e |> json_of_string_opt))
    else
      let (>>=) = Stdlib.Option.bind in
      let e = Utils.select_any_of f.field_selectors soup >>= get_content f in
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
  let (>>=) = CCOpt.(>>=) in
  let string_of_elem selector soup =
    Utils.select_any_of selector soup >>= string_of_elem settings.index_strip_tags
  in
  {
    url = env.page_url;
    page_file = env.page_file;
    nav_path = env.nav_path;
    title = string_of_elem settings.index_title_selector soup;
    excerpt = string_of_elem settings.index_excerpt_selector soup;
    (* Try to extract only the texts from the date element,
       to account for things like <em>1970</em>-01-01.
       Probably futile and redundant, but at least no one can say it's not trying.
     *)
    date = Utils.select_any_of settings.index_date_selector soup >>= Utils.get_element_text;
    author = string_of_elem settings.index_author_selector soup;
    custom_fields = get_custom_fields settings.index_strip_tags settings.index_custom_fields soup
  }

(** Compares entries by their dates according to these rules:
    1. Entries without known dates are equal
    2. Entries with a known date are newer than those without
    3. Of entries with known dates, ones with later dates are newer (who could guess!)
  *)
let compare_entries settings l r =
  let (>>=) = CCOpt.(>>=) in
  let get_date entry =
    try
      entry.date >>=
      (fun s -> Some (CalendarLib.Printer.Date.from_fstring settings.index_date_format s))
    with Invalid_argument msg ->
      let () = Logs.warn @@ fun m -> m "Could not parse date: %s" msg in
      None
  in
  let compare_dates l_date r_date =
    match l_date, r_date with
    | None, None -> 0
    | Some _, None -> 1
    | None, Some _ -> -1
    | Some l_date, Some r_date ->
      CalendarLib.Date.compare l_date r_date
  in
  let l_date = get_date l in
  let r_date = get_date r in
  let result = compare_dates l_date r_date in
  if settings.newest_entries_first then (~- result) else result

let json_of_entry e =
  let fields = ["title", e.title; "date", e.date; "author", e.author; "excerpt", e.excerpt] in
  let fields = List.map (fun (k, v) -> (k, json_of_string_opt v)) fields in
  let fields = ("url", `String e.url) :: fields in
  let fields = ("page_file", `String e.page_file) :: fields in
  let fields = ("nav_path", `A (List.map (fun x -> `String x) e.nav_path)) :: fields in
  let fields = List.append fields e.custom_fields in
  `O fields

let json_of_entries settings es =
  let es = List.sort (compare_entries settings) es in
  `A (List.map json_of_entry es)

let json_string_of_entries ?(minify=false) settings es =
  json_of_entries settings es |> Ezjsonm.to_string ~minify:minify

(** Renders an index using built-in Mustache templates *)
let render_index template settings soup entries =
  let () = Logs.info @@ fun m -> m "Generating section index" in
  let strict = not settings.ignore_template_errors in
  try
    let () =
      (* Debug output *)
      if settings.debug then
      Logs.debug @@ fun m -> m "Index data (pretty-printed): %s" (json_string_of_entries ~minify:false settings entries)
    in
    let entries = List.sort (compare_entries settings) entries in
    let entries = List.map (fun e -> json_of_entry e |> Mustache.render ~strict:strict template |> Soup.parse) entries in
    let () = List.iter (Soup.append_child soup) entries in
    Ok ()
  with
  | Mustache_types.Missing_variable s | Mustache_types.Missing_section s | Mustache_types.Missing_partial s ->
    let err = Printf.sprintf "Failed to render index item template: undefined variable or section \"%s\"" s in
    Error err
  | _ -> Error ("Failed to render index: invalid template")

let run_index_processor settings cmd ic index =
  (* Minification is intentional, newline is used as end of input *)
  let json = json_string_of_entries ~minify:true settings index in
  let () = Logs.info @@ fun m -> m "Calling index processor %s" cmd in
  let output = Utils.get_program_output ~input:(Some json) cmd [| |] in
  begin
    match output with
    | Error _ as e -> e
    | Ok output -> Ok (Soup.append_child ic (Soup.parse output))
  end

let insert_index settings soup index view =
  let index_container = Soup.select_one view.index_selector soup in
  match index_container with
  | None ->
    let () = Logs.warn @@ fun m -> m "Page doesn't have an element matching selector \"%s\", ignoring index view \"%s\"" view.index_selector view.index_view_name in
    Ok ()
  | Some ic ->
    begin
      match view.index_processor with
      | Defaults.BuiltInTemplate tmpl -> render_index tmpl settings ic index
      | Defaults.ExternalIndexer cmd -> run_index_processor settings cmd ic index
    end

let insert_indices settings soup index =
  Utils.iter ~ignore_errors:(not settings.strict) (insert_index settings soup index) settings.index_views

let index_extraction_should_run settings page_file =
  if not (Utils.profile_matches settings.index_profile settings.build_profile) then
    let () = Logs.debug @@ fun m -> m "Index extraction is disabled by build profile options" in false
  else begin
    if Path_options.page_included settings.index_path_options settings.site_dir page_file then true
    else
      let () = Logs.debug @@ fun m -> m "Page %s excluded from indexing by page/section/regex options" page_file in
      false
  end
