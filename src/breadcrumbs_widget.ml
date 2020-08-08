(* Breadcrumbs *)

open Defaults

let make_breadcrumbs nav_path bc_tmpl prepend append between =
  let rec aux xs bc_soup acc_href =
    let box_string s = Template.jingoo_of_json (`String s) in
    match xs with
    | [] -> ()
    | x :: xs ->
      (* href for each next level accumulates, e.g. section, section/subsection... *)
      let acc_href = Printf.sprintf "%s/%s" acc_href x in
      let bc_link = Template.render bc_tmpl ["name", box_string x; "url", box_string acc_href] |> Soup.parse in
      let () = Soup.append_root bc_soup bc_link in
      (* Fixup: don't insert the "between" after the last element *)
      let () = if (List.length xs) >= 1 then Soup.append_root bc_soup (Soup.parse between) in
      aux xs bc_soup acc_href
  in
  let bc_soup = Soup.create_soup () in
  (* XXX: reusing a soup for append_child doesn't seem to work as of 0.6.3,
     nodes retain their context forever.
     this is why they are parsed into a new soup every time *)
  let () = Soup.append_root bc_soup (Soup.parse prepend) in
  let () = aux nav_path bc_soup "" in
  let () = Soup.append_root bc_soup (Soup.parse append) in
  bc_soup

let check_breadcrumb_template tmpl_str =
  try let _ = Template.of_string tmpl_str in Ok ()
  with _ -> Error "Failed to parse a breadcrumb template"

let breadcrumbs env config soup =
  let valid_options = List.append Config.common_widget_options
    ["selector"; "min_depth"; "append"; "prepend"; "between"; "breadcrumb_template"; "action"] in
  let () = Config.check_options valid_options config "widget \"breadcrumbs\"" in
  let min_depth = Config.get_int_default 1 "min_depth" config in
  let selector = Config.get_string_result "Missing required option \"selector\"" "selector" config in
  let action = Config.get_string_default "append_child" "action" config in
  match selector with
  | Error _ as e -> e
  | Ok selector ->
    let container = Soup.select_one selector soup in
    let (let*) = Stdlib.Result.bind in
    begin
      match container with
      | None ->
        let () = Logs.debug @@ fun m -> m "Page has no elements matching selector \"%s\", nowhere to insert the breadcrumbs" selector in
        Ok ()
      | Some container ->
        let path_length = List.length env.nav_path in
        if path_length < min_depth then Ok () else
        let bc_tmpl_str = Config.get_string_default "<a href=\"{{url}}\">{{name}}</a>" "breadcrumb_template" config in
        let* _  = check_breadcrumb_template bc_tmpl_str in
        let bc_tmpl = Template.of_string bc_tmpl_str in
        let prepend = Config.get_string_default "" "prepend" config in
        let append = Config.get_string_default "" "append" config in
        let between = Config.get_string_default "" "between" config in
        let breadcrumbs = make_breadcrumbs env.nav_path bc_tmpl prepend append between in
        Ok (Html_utils.insert_element action container breadcrumbs)
    end
