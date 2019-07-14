open Defaults

let bind = CCResult.(>>=)

type 'a index_entry = {
  url: string;
  nav_path: string list;
  title: 'a Soup.node option;
  excerpt: 'a Soup.node option;
  date: 'a Soup.node option;
  author: 'a Soup.node option;
}

let get_entry settings url nav_path soup =
  {
    url = url;
    nav_path = nav_path;
    title = Soup.select_one settings.index_title_selector soup;
    excerpt = Soup.select_one settings.index_excerpt_selector soup;
    date = Soup.select_one settings.index_date_selector soup;
    author = Soup.select_one settings.index_author_selector soup;
  }

(** Compares entries by their dates according to these rules:
    1. Entries without known dates are equal
    2. Entries with a known date are newer than those without
    3. Of entries with known dated, ones with a later date are newer (who could guess!)
  *)
let compare_entries settings l r =
  let (>>=) = CCOpt.(>>=) in
  let get_date entry =
    try
      entry.date >>= Utils.get_element_text >>=
      (fun s -> Some (CalendarLib.Printer.Date.from_fstring settings.index_date_format s))
    with Invalid_argument msg ->
      let () = Logs.warn @@ fun m -> m "Could not parse date: %s" msg in
      None
  in
  let l_date = get_date l in
  let r_date = get_date r in
  match l_date, r_date with
  | None, None -> 0
  | Some _, None -> 1
  | None, Some _ -> -1
  | Some l_date, Some r_date ->
    CalendarLib.Date.compare l_date r_date

let make_title_link entry =
  match entry.title with
  | None -> None
  | Some title ->
    let title_link = Soup.create_element ~attributes:["href", entry.url] "a" in
    Soup.append_child title_link title;
    Some title_link

let make_entry tmpl entry =
  let tmpl = Soup.parse tmpl in
  let entry_html = Soup.select_one "*" tmpl |> Soup.require in
  let title_link = make_title_link entry in
  Utils.append_child entry_html title_link;
  Utils.append_child entry_html title_link;
  Utils.append_child entry_html entry.date;
  Utils.append_child entry_html entry.author;
  Utils.append_child entry_html entry.excerpt;
  entry_html

let add_index settings soup entries =
  let entries = List.sort (compare_entries settings) entries in
  let entries = List.map (make_entry settings.index_item_template) entries in
  List.iter (Soup.append_child soup) entries

(* JSON conversion. Not sure if ppx_deriving_yojson would be easier here *)
let json_of_element e =
  match e with
  | None -> `Null
  | Some e ->
    begin
      let text =  Utils.inner_html e in
      match text with
      | None -> `Null
      | Some t -> `String t
    end

let json_of_entry e =
  let fields = ["title", e.title; "date", e.date; "author", e.author; "excerpt", e.excerpt] in
  let fields = List.map (fun (k, v) -> (k, json_of_element v)) fields in
  let fields = ("url", `String e.url) :: fields in
  let fields = ("nav_path", `List (List.map (fun x -> `String x) e.nav_path)) :: fields in
  `Assoc fields

let json_of_entries settings es =
  let es = List.sort (compare_entries settings) es in
  (* XXX: to_string instead of pretty_to_string is intentional,
     newline is used as an end of input marker when passing
     the data to index processor's stdin *)
  `List (List.map json_of_entry es) |> Yojson.Safe.to_string
