open Defaults

let bind = CCResult.(>>=)

type 'a index_entry = {
  url: string;
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
  let get_field f soup =
  if f.select_all then
    `A (Soup.select f.field_selector soup |> Soup.to_list |> List.map (fun e -> string_of_elem strip_tags e |> json_of_string_opt))
  else let e = Soup.select_one f.field_selector soup in
  match e with
    | None -> `Null
    | Some e -> `String (string_of_elem strip_tags e |> CCOpt.get_or ~default:"")
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
  let fields = ("nav_path", `A (List.map (fun x -> `String x) e.nav_path)) :: fields in
  let fields = List.append fields e.custom_fields in
  `O fields

let json_of_entries settings es =
  let es = List.sort (compare_entries settings) es in
  (* XXX: to_string instead of pretty_to_string is intentional,
     newline is used as an end of input marker when passing
     the data to index processor's stdin *)
  `A (List.map json_of_entry es) |> Ezjsonm.to_string

let add_index settings soup entries =
  let () = Logs.info @@ fun m -> m "Generating section index" in
  let strict = not settings.ignore_template_errors in
  let tmpl = settings.index_item_template in
  try
    let () = if settings.debug then Logs.debug @@ fun m -> m "Index data: %s" (json_of_entries settings entries) in
    let entries = List.sort (compare_entries settings) entries in
    let entries = List.map (fun e -> json_of_entry e |> Mustache.render ~strict:strict tmpl |> Soup.parse) entries in
    let () = List.iter (Soup.append_child soup) entries in
    Ok ()
  with
  | Mustache_types.Missing_variable s | Mustache_types.Missing_section s | Mustache_types.Missing_partial s ->
    let err = Printf.sprintf "Failed to render index item template: undefined variable or section \"%s\"" s in
    Error err
  | _ -> Error ("Failed to render index: invalid template")
