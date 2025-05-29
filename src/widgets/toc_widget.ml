module OH = Otoml.Helpers

open Common

let (let*) = Result.bind

(** The table of contents widget *)

type toc_settings = {
  min_level: int;
  max_level: int;
  max_heading_link_level: int;
  toc_class: string option;
  toc_class_levels: bool;
  numbered_list: bool;
  link_here: bool;
  link_here_text: string;
  link_here_class: string option;
  link_here_append: bool;
  use_text: bool;
  use_slugs: bool;
  soft_slug: bool;
  slug_regex: string option;
  slug_replacement: string option;
  slug_force_lowercase: bool;
  strip_tags: bool;
  valid_html: bool;
  min_headings: int;
  ignore_heading_selectors: string list;
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
        if settings.use_slugs then
          (* soft_slug is a shortcut for "only replace whitespace with hyphens" *)
          let regex = if settings.soft_slug then (Some {|\s+|}) else settings.slug_regex in
          let lowercase = if settings.soft_slug then false else settings.slug_force_lowercase in
          try Utils.slugify
            ~lowercase:lowercase
            ~regex:regex
            ~sub:settings.slug_replacement
            t
          with _ ->
            soupault_error @@ Printf.sprintf "Invalid regex in the slug_regex option: '%s'"
              (Option.value ~default:"" regex)
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
let add_item settings heading container =
  let li = Soup.create_element "li" in
  (* It's safe to unsafely unwrap options here because this function
     is only called for headings that must have had their ids set earlier. *)
  let heading_id = Soup.attribute "id" heading |> Option.get in
  let h_link = Soup.create_element ~attributes:["href", "#" ^ heading_id] "a" in
  let h_content = Html_utils.child_nodes heading in
  (* Strip tags if configured *)
  let h_content =
    if settings.strip_tags then Html_utils.get_element_text h_content |> Option.value ~default:"" |> Soup.parse
    else h_content
  in
  Soup.append_child h_link h_content;
  Soup.append_child li h_link;
  Soup.append_child container li;
  (* Return the generated <li> element *)
  li

let make_heading_linkable settings counter heading =
  if (Html_utils.get_heading_level heading) <= settings.max_heading_link_level then
  (* Set heading id to make it possible to link directly to a section *)
  let heading_id = get_heading_id settings counter heading in
  Soup.set_attribute "id" heading_id heading

let add_section_link settings heading =
  let heading_level = Html_utils.get_heading_level heading in
  if (heading_level <= settings.max_heading_link_level) &&
     (heading_level >= settings.min_level) then
  let heading_id = Soup.attribute "id" heading |> Option.get in
  let link_text = Soup.parse settings.link_here_text in
  let link_here = Soup.create_element ~attributes:["href", "#" ^ heading_id] "a" in
  let () = Soup.append_child link_here link_text in
  Html_utils.add_class settings.link_here_class link_here;
  if settings.link_here_append then Soup.append_child heading link_here
  else Soup.prepend_child heading link_here

let make_toc_container settings level =
  let tag = if settings.numbered_list then "ol" else "ul" in
  let toc_list = Soup.create_element tag in
  let toc_class = make_toc_class settings level in
  Html_utils.add_class toc_class toc_list;
  toc_list

let level_matches settings h =
  let level = Html_utils.get_heading_level h in
  (level <= settings.max_level) && (level >= settings.min_level)

let ignored_heading settings soup h =
  let res = Html_utils.matches_any_of settings.ignore_heading_selectors soup h in
  match res with
  | true ->
    let () = Logs.debug @@ fun m -> m "Heading %s is ignored due to ignore_selector settings" (Soup.to_string h) in
    true
  | false -> false

let rec _make_toc settings depth counter parent tree =
  let heading = Toc_tree.(tree.value) in
  let children = Toc_tree.(tree.children) in
  let level = Html_utils.get_heading_level heading in
  if level > settings.max_level then
    Logs.debug @@ fun m -> m "Heading %s is ignored because its level exceeds max_level (%d)"
      (Soup.to_string heading) settings.max_level
  else if level < settings.min_level then
    let () = Logs.debug @@ fun m -> m "Heading %s is ignored because its level is below min_level (%d), processings its sub-headings (if any)"
      (Soup.to_string heading) settings.min_level
    in
    List.iter (_make_toc settings depth counter parent) children
  else let item = add_item settings heading parent in
  match children with
  | [] -> ()
  | _ ->
    (* Avoid inserting ul/ol ToC containers that are doomed to stay empty
       because all child headings are deeper than the max_level.
       Better keep the HTML clean.
     *)
    if not (List.exists (level_matches settings) (List.map (fun c -> Toc_tree.(c.value)) children)) then () else
    let container = make_toc_container settings depth in
    (* According to the HTML specs, and contrary to the popular opinion,
       a <ul> or <ol> cannot contain another <ul> or <ol>.
       Nested lists must be inside <li> elements.
       With valid_html the user can force that behaviour.
     *)
    if settings.valid_html then Soup.append_child item container
    else Soup.append_child parent container;
    List.iter (_make_toc settings (depth + 1) counter container) children

let toc _ config page =
  let soup = page.element_tree in
  let valid_options = List.append Config.common_widget_options [
    (* General options *)
    "selector"; "action"; "strip_tags";
    (* Level options *)
    "min_level"; "max_level"; "max_heading_link_level";
    (* Styling and layout *)
    "toc_list_class"; "toc_class_levels";
    "numbered_list"; "valid_html";
    (* Settings for the section links placed next to headings *)
    "heading_links"; "heading_link_text"; "heading_link_class"; "heading_links_append";
    (* Slugification options *)
    "use_heading_text"; "use_heading_slug";
    "soft_slug"; "slug_regex"; "slug_replacement_string";
    "slug_force_lowercase";
    (* Exclude headings that match specific selectors *)
    "ignore_heading_selectors"
  ]
  in
  let () = Config.check_options valid_options config {|widget "toc"|} in
  let max_level = Config.find_integer_or ~default:6 config ["max_level"] in
  let settings = {
    min_level = Config.find_integer_or ~default:1 config ["min_level"];
    max_level = max_level;
    max_heading_link_level =
      (let lvl = Config.find_integer_or ~default:max_level config ["max_heading_link_level"] in
      if lvl < max_level then begin
        let () = Logs.warn @@ fun m -> m "max_heading_level cannot be lower than max_level (%d), forcing to %d"
          max_level max_level
        in max_level
      end
      else lvl);
    toc_class = OH.find_string_opt config ["toc_list_class"];
    toc_class_levels = Config.find_bool_or ~default:false config ["toc_class_levels"];
    numbered_list = Config.find_bool_or ~default:false config ["numbered_list"];
    link_here = Config.find_bool_or ~default:false config ["heading_links"];
    link_here_text = Config.find_string_or ~default:"#" config ["heading_link_text"];
    link_here_class = OH.find_string_opt config ["heading_link_class"];
    link_here_append = Config.find_bool_or ~default:false config ["heading_links_append"];
    use_text = Config.find_bool_or ~default:false config ["use_heading_text"];
    use_slugs = Config.find_bool_or ~default:false config ["use_heading_slug"];
    soft_slug = Config.find_bool_or ~default:false config ["soft_slug"];
    slug_regex = OH.find_string_opt config ["slug_regex"];
    slug_replacement = OH.find_string_opt config ["slug_replacement_string"];
    slug_force_lowercase = Config.find_bool_or ~default:true config ["slug_force_lowercase"];
    strip_tags = Config.find_bool_or ~default:false config ["strip_tags"];
    valid_html = Config.find_bool_or ~default:false config ["valid_html"];
    min_headings = Config.find_integer_or ~default:0 config ["min_headings"];
    ignore_heading_selectors = Config.find_strings_or ~default:[] config ["ignore_heading_selectors"];
  } in
  let selectors = Config.find_strings config ["selector"] in
  let action = OH.find_string_opt config ["action"] in
  begin
    let container = Html_utils.select_any_of selectors soup in
    match container with
    | None ->
      Utils.no_container_action selectors "nowhere to insert the table of contents"
    | Some container ->
      begin
        let counter = make_counter 0 in
        let headings = Html_utils.find_headings soup in
        let headings = List.filter (fun e -> not @@ ignored_heading settings soup e) headings in
        (* Don't do anything if the page has fewer headings than set by the min_headings option. *)
        if ((List.length headings) < settings.min_headings) then () else
        let () = List.iter (fun h -> make_heading_linkable settings counter h) headings in
        let headings_tree = headings |> Toc_tree.from_list Html_utils.get_heading_level in
        match headings_tree with
        | [] ->
          Logs.debug @@ fun m -> m "Page has no headings, nothing to build a ToC from"
        | _ ->
          let toc_container = make_toc_container settings 1 in
          let _ = List.iter (_make_toc settings 2 counter toc_container) headings_tree in
          Html_utils.insert_element action container toc_container;
          if settings.link_here then List.iter (fun h -> add_section_link settings h) headings
      end
  end
