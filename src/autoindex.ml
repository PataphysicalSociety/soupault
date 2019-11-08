open Defaults

let bind = CCResult.(>>=)

type 'a index_entry = {
  url: string;
  nav_path: string list;
  title: string option;
  excerpt: string option;
  date: string option;
  author: string option;
  custom_fields : (string * Yojson.Safe.t) list
}

let rec get_custom_fields fields soup =
  let get_field f soup =
  if f.select_all then
    `List (Soup.select f.field_selector soup |> Soup.to_list |> List.map (fun e -> `String (Utils.inner_html e)))
  else let e = Soup.select_one f.field_selector soup in
  match e with
    | None -> `Null
    | Some e -> `String (Utils.inner_html e)
  in
  match fields with
  | [] -> []
  | f :: fs ->
    let field = (f.field_name, get_field f soup) in
    field :: (get_custom_fields fs soup)

let get_entry settings url nav_path soup =
  let (>>=) = CCOpt.(>>=) in
  let string_of_elem selector soup =
    Utils.select_any_of selector soup >>= (fun x -> Some (Utils.inner_html x))
  in
  {
    url = url;
    nav_path = nav_path;
    title = string_of_elem settings.index_title_selector soup;
    excerpt = string_of_elem settings.index_excerpt_selector soup;
    (* Try to extract only the texts from the date element,
       to account for things like <em>1970</em>-01-01.
       Probably futile and redundant, but at least no one can say it's not trying.
     *)
    date = Utils.select_any_of settings.index_date_selector soup >>= Utils.get_element_text;
    author = string_of_elem settings.index_author_selector soup;
    custom_fields = get_custom_fields settings.index_custom_fields soup
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

let make_title_link entry =
  match entry.title with
  | None -> None
  | Some title ->
    let title_link = Soup.create_element ~attributes:["href", entry.url] "a" in
    Soup.append_child title_link (Soup.parse title);
    Some title_link

let make_entry tmpl entry =
  let tmpl = Soup.parse tmpl in
  let entry_html = Soup.select_one "*" tmpl |> Soup.require in
  let title_link = make_title_link entry in
  Utils.append_child entry_html title_link;
  entry_html

let add_index settings soup entries =
  let entries = List.sort (compare_entries settings) entries in
  let entries = List.map (make_entry settings.index_item_template) entries in
  List.iter (Soup.append_child soup) entries

(* JSON conversion. Not sure if ppx_deriving_yojson would be easier here *)
let json_of_string_opt s =
  match s with
  | None -> `Null
  | Some s -> `String s

let json_of_entry e =
  let fields = ["title", e.title; "date", e.date; "author", e.author; "excerpt", e.excerpt] in
  let fields = List.map (fun (k, v) -> (k, json_of_string_opt v)) fields in
  let fields = ("url", `String e.url) :: fields in
  let fields = ("nav_path", `List (List.map (fun x -> `String x) e.nav_path)) :: fields in
  let fields = List.append fields e.custom_fields in
  `Assoc fields

let json_of_entries settings es =
  let es = List.sort (compare_entries settings) es in
  (* XXX: to_string instead of pretty_to_string is intentional,
     newline is used as an end of input marker when passing
     the data to index processor's stdin *)
  `List (List.map json_of_entry es) |> Yojson.Safe.to_string
