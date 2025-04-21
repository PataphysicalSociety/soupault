(* Breadcrumbs *)

open Defaults
open Soupault_common

let make_breadcrumbs nav_path bc_tmpl prepend append between =
  let rec aux xs bc_soup acc_href =
    let box_string s = Template.jingoo_of_json (`String s) in
    match xs with
    | [] -> ()
    | x :: xs ->
      (* href for each next level accumulates, e.g. section, section/subsection... *)
      let acc_href = Printf.sprintf "%s/%s" acc_href x in
      let bc_link = Template.render bc_tmpl ["name", box_string x; "url", box_string acc_href] |> Soup.parse in
      let () =
        Soup.append_root bc_soup bc_link;
        (* Fixup: only insert the "between" bit if it's not the last element. *)
        if (List.length xs) >= 1 then Soup.append_root bc_soup (Soup.parse between)
      in
      aux xs bc_soup acc_href
  in
  let bc_soup = Soup.create_soup () in
  let () =
    (* XXX: reusing a soup for append_child doesn't seem to work as of 0.6.3,
       nodes retain their context forever.
       this is why they are parsed into a new soup every time *)
    Soup.append_root bc_soup (Soup.parse prepend);
    aux nav_path bc_soup "";
    Soup.append_root bc_soup (Soup.parse append)
  in
  bc_soup

let breadcrumbs _ config _ page =
  let soup = page.element_tree in
  let valid_options = List.append Config.common_widget_options
    ["selector"; "min_depth"; "append"; "prepend"; "between"; "breadcrumb_template"; "action"] in
  let () = Config.check_options valid_options config {|widget "breadcrumbs"|} in
  let min_depth = Config.find_integer_or ~default:1 config ["min_depth"] in
  let selectors = Config.find_strings config ["selector"] in
  let action = Otoml.Helpers.find_string_opt config ["action"] in
  let container = Html_utils.select_any_of selectors soup in
  begin match container with
  | None ->
    Utils.no_container_action selectors "nowhere to insert the breadcrumbs"
  | Some container ->
    let path_length = List.length page.nav_path in
    if path_length < min_depth then () else
    let bc_tmpl_str = Config.find_string_or ~default:{|<a href="{{url}}">{{name}}</a>|} config ["breadcrumb_template"] in
    let bc_tmpl =
      (try Template.of_string bc_tmpl_str
       with _ -> widget_error
         "failed to parse breadcrumb template\
         (consult Jingoo documentation for a syntax reference)")
    in
    let prepend = Config.find_string_or ~default:"" config ["prepend"] in
    let append = Config.find_string_or ~default:"" config ["append"] in
    let between = Config.find_string_or ~default:"" config ["between"] in
    let breadcrumbs = make_breadcrumbs page.nav_path bc_tmpl prepend append between in
    Html_utils.insert_element action container breadcrumbs
  end
