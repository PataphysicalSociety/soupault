(** The table of contents widget *)

type toc_settings = {
  min_level: int;
  max_level: int;
  toc_class: string option;
  toc_class_levels: bool;
  numbered_list: bool;
  link_here: bool;
  link_here_text: string;
  link_here_class: string option;
  link_here_append: bool;
  use_text: bool;
  use_slugs: bool;
}

let slugify s = 
  Re.Str.global_replace (Re.Str.regexp "[^a-zA-Z0-9\\-]") "-" s |>
  String.lowercase_ascii

let is_heading e =
  match (Soup.name e) with
  | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" -> true
  | _ -> false

let find_headings soup =
  let open Soup in
  soup |> descendants |> elements |> filter is_heading |> to_list

let get_heading_level e = String.sub (Soup.name e) 1 1 |> int_of_string

let make_counter seed =
  let counter = ref seed in
  fun () -> incr counter; !counter

let get_heading_id settings counter heading =
  let id = Soup.attribute "id" heading in
  match id with
  | Some id -> id
  | None ->
    if not (settings.use_slugs || settings.use_text)
    then counter () |> string_of_int else
    let text =  Utils.get_element_text heading in
    begin
      match text with
      | None -> counter () |> string_of_int
      | Some t ->
        if settings.use_slugs then slugify t
        else t
    end

let make_toc_class settings level =
  match	settings.toc_class with
  | None -> None
  | Some _class ->
    let _class = 
      if settings.toc_class_levels then Printf.sprintf "%s-%d" _class level
      else _class
    in Some _class

(** Adds a link to the ToC list and sets the identifier
    of the heading so that the link actually works *)
let add_item settings counter heading container =
  let li = Soup.create_element "li" in
  let heading_id = get_heading_id settings counter heading in
  let h_link = Soup.create_element ~attributes:["href", "#" ^ heading_id] "a" in
  let h_content = Utils.child_nodes heading in
  Soup.append_child h_link h_content;
  Soup.append_child li h_link;
  Soup.append_child container li;
  Soup.set_attribute "id" heading_id heading;
  (* Add a link to the current heading if requested *)
  if settings.link_here then
  let link_here = Soup.create_element ~attributes:["href", "#" ^ heading_id] ~inner_text:settings.link_here_text "a" in
  Utils.add_class settings.link_here_class link_here;
  if settings.link_here_append then Soup.append_child heading link_here
  else Soup.prepend_child heading link_here

let make_toc_container settings level =
  let tag = if settings.numbered_list then "ol" else "ul" in
  let toc_list = Soup.create_element tag in
  let toc_class = make_toc_class settings level in
  Utils.add_class toc_class toc_list;
  toc_list

let rec _make_toc settings counter soup container cur_level headings =
  match headings with
  | [] -> []
  | h :: hs ->
    begin
      let level = get_heading_level h in
      if (level < settings.min_level) || (level > settings.max_level)
      then _make_toc settings counter soup container cur_level hs 
      else match level, cur_level with
      | _ when level = cur_level ->
        (* Same level, just add a new item and go ahead *)
        add_item settings counter h container;
        _make_toc settings counter soup container cur_level hs
      | _ when level > cur_level ->
        (* Hello Mr. Tyler, going down?
           This is a deeper level. We call _make_toc on the list tail --
           it will return remaining items list when the level increases, then call it again
           on the list it returns.
         *)
        let container' = make_toc_container settings level in
        let () = add_item settings counter h container' in 
        let hs' = _make_toc settings counter soup container' level hs in
        let () = Soup.append_child container container' in
        _make_toc settings counter soup container cur_level hs'
      | _ ->
        (* The level goes up. We need to return this heading
           and all remaining headings to the caller at the upper level *)
        h :: hs
    end

let toc _ config soup =
  let valid_options = List.append Config.common_widget_options
    ["selector"; "min_level"; "max_level"; "toc_list_class"; "toc_class_levels"; "numbered_list";
     "heading_links"; "heading_link_text"; "heading_link_class"; "heading_links_append";
     "use_heading_text"; "use_heading_slug"; "use_header_text"; "use_header_slug"]
  in
  let () = Config.check_options valid_options config "widget \"toc\"" in
  let settings = {
    min_level = Config.get_int_default 1 "min_level" config;
    max_level = Config.get_int_default 6 "max_level" config;
    toc_class = Config.get_string "toc_list_class" config;
    toc_class_levels = Config.get_bool_default false "toc_class_levels" config;
    numbered_list = Config.get_bool_default false "numbered_list" config;
    link_here = Config.get_bool_default false "heading_links" config;
    link_here_text = Config.get_string_default "#" "heading_link_text" config;
    link_here_class = Config.get_string "heading_link_class" config;
    link_here_append = Config.get_bool_default false "heading_links_append" config;
    use_text = Config.get_bool_default false "use_heading_text" config;
    use_slugs = Config.get_bool_default false "use_heading_slug" config;
  } in
  let selector = Config.get_string_result "Missing required option \"selector\"" "selector" config in
  let () =
    Utils.deprecation_warning Config.get_bool "use_header_text" "use \"use_heading_text\" instead" config;
    Utils.deprecation_warning Config.get_bool "use_header_slug" "use \"use_heading_slug\" instead" config
  in
  match selector with
  | Error _ as e -> e
  | Ok selector ->
    let container = Soup.select_one selector soup in
    if CCOpt.is_none container then Ok () else
    let container = Utils.unwrap_option container in
    begin
      let counter = make_counter 0 in
      let toc_container = make_toc_container settings settings.min_level in
      let headings = find_headings soup in
      let _ = _make_toc settings counter soup toc_container settings.min_level headings in
      Ok (Soup.append_child container toc_container)
    end
