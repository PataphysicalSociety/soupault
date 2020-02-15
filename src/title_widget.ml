let (>>=) = CCOpt.(>>=)

(* Title *)
let set_title _ config soup =
  let make_title_string default prepend append title_opt =
    (* If title is not given, return the default title
       without appending or prepending anything, since that would look weird *)
    match title_opt with
    | None -> default
    | Some title ->
      let title = Printf.sprintf "%s%s" prepend title in
      let title = Printf.sprintf "%s%s" title append in
      title
  in
  (* Retrieve config options. The "selector" option means title source element, by default the first <h1> *)
  let valid_options = List.append Config.common_widget_options ["selector"; "default"; "prepend"; "append"] in
  let () = Config.check_options valid_options config "widget \"title\"" in
  let selectors = Config.get_strings_relaxed ~default:["h1"] "selector" config in
  let prepend = Config.get_string_default "" "prepend" config in
  let append = Config.get_string_default "" "append" config in
  let default_title = Config.get_string_default "" "default" config in
  (* Now to setting the title *)
  let title_node = Soup.select_one "title" soup in
  match title_node with
  | None ->
    let () = Logs.debug @@ fun m -> m "Page has no <title> node, assuming you don't want to set it" in
    Ok ()
  | Some title_node ->
    let title_string =
      Utils.select_any_of selectors soup >>= Utils.get_element_text
        |> make_title_string default_title prepend append in
    (* XXX: Both Soup.create_text and Soup.create_element ~inner_text:... escape special characters
       instead of expanding entities, so "&mdash;" becomes "&amp;mdash", which is not what we want.
       Soup.parse expands them, which is why it's used here *)
    let new_title_node = Printf.sprintf "<title>%s</title>" title_string |> Soup.parse in
    let () = Soup.replace title_node new_title_node in
    Ok ()
