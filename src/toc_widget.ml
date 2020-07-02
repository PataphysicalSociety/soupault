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
  strip_tags: bool;
  valid_html: bool;
}

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
    let text =  Html_utils.get_element_text heading in
    begin
      match text with
      | None -> counter () |> string_of_int
      | Some t ->
        if settings.use_slugs then Utils.slugify t
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
  let h_content = Html_utils.child_nodes heading in
  (* Strip tags if configured *)
  let h_content =
    if settings.strip_tags then Html_utils.get_element_text h_content |> CCOpt.get_or ~default:"" |> Soup.parse
    else h_content
  in
  Soup.append_child h_link h_content;
  Soup.append_child li h_link;
  Soup.append_child container li;
  Soup.set_attribute "id" heading_id heading;
  (* Add a link to the current heading if requested *)
  if settings.link_here then begin
    let link_text = Soup.parse settings.link_here_text in
    let link_here = Soup.create_element ~attributes:["href", "#" ^ heading_id] "a" in
    let () = Soup.append_child link_here link_text in
    Html_utils.add_class settings.link_here_class link_here;
    if settings.link_here_append then Soup.append_child heading link_here
    else Soup.prepend_child heading link_here
  end;
  (* Return the generated <li> element *)
  li

let make_toc_container settings level =
  let tag = if settings.numbered_list then "ol" else "ul" in
  let toc_list = Soup.create_element tag in
  let toc_class = make_toc_class settings level in
  Html_utils.add_class toc_class toc_list;
  toc_list

let rec _make_toc settings depth counter parent tree =
  let heading = Rose_tree.(tree.value) in
  let children = Rose_tree.(tree.children) in
  let level = Html_utils.get_heading_level heading in
  if level > settings.max_level then () else
  if level < settings.min_level then List.iter (_make_toc settings depth counter parent) children else
  let item = add_item settings counter heading parent in
  match children with
  | [] -> ()
  | _ ->
    let container = make_toc_container settings depth in
    (* According to the HTML specs, and contrary to the popular opinion,
       a <ul> or <ul> cannot contain another <ul> or <ul>.
       Nested lists must be inside its <li> elements.
       With valid_html the user can force that behaviour.
     *)
    if settings.valid_html then Soup.append_child item container
    else Soup.append_child parent container;
    List.iter (_make_toc settings (depth + 1) counter container) children

let toc _ config soup =
  let valid_options = List.append Config.common_widget_options
    ["selector"; "min_level"; "max_level"; "toc_list_class"; "toc_class_levels"; "numbered_list";
     "heading_links"; "heading_link_text"; "heading_link_class"; "heading_links_append"; "valid_html";
     "use_heading_text"; "use_heading_slug"; "use_header_text"; "use_header_slug"; "strip_tags"; "action"]
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
    strip_tags = Config.get_bool_default false "strip_tags" config;
    valid_html = Config.get_bool_default false "valid_html" config;
  } in
  let selector = Config.get_string_result "Missing required option \"selector\"" "selector" config in
  let action = Config.get_string_default "append_child" "action" config in
  let () =
    Utils.deprecation_warning Config.get_bool "use_header_text" "use \"use_heading_text\" instead" config;
    Utils.deprecation_warning Config.get_bool "use_header_slug" "use \"use_heading_slug\" instead" config
  in
  match selector with
  | Error _ as e -> e
  | Ok selector ->
    begin
      let container = Soup.select_one selector soup in
      match container with
      | None ->
        let () = Logs.debug @@ fun m -> m "Page has no elements matching selector \"%s\", nowhere to insert the ToC" selector in
        Ok ()
      | Some container ->
      begin
        let counter = make_counter 0 in
        let toc_container = make_toc_container settings 1 in
        let headings = Html_utils.find_headings soup |> Rose_tree.from_list Html_utils.get_heading_level in
        let _ = List.iter (_make_toc settings 2 counter toc_container) headings in
        Ok (Html_utils.insert_element action container toc_container)
      end
    end
