(* Option monad *)
let (>>=) = CCOpt.(>>=)

(* Built-in widgets *)
let set_title config soup =
  (* Retrieve config options. The "selector" option means title source element, by default the first <h1> *)
  let selector = Config.get_string_default "h1" "selector" config in
  let prepend = Config.get_string_default "" "prepend" config in
  let append = Config.get_string_default "" "append" config in
  let default_title = Config.get_string_default "" "default" config in
  (* Now to setting the title *)
  let title_node = Soup.select_one "title" soup in
  match title_node with
  | None ->
    let () = Logs.info @@ fun m -> m "Page has no <title> node, assuming you don't want to set it" in
    Ok ()
  | Some title_node ->
    let title_string = Soup.select_one selector soup >>= Soup.leaf_text |> Config.default default_title in
    let title_string = Printf.sprintf "%s%s" prepend title_string in
    let title_string = Printf.sprintf "%s%s" title_string append in
    (* XXX: Both Soup.create_text and Soup.create_element ~inner_text:... escape special characters
       instead of expanding entities, so "&mdash;" becomes "&amp;mdash", which is not what we want.
       Soup.parse expands them, which is why it's used here *)
    let new_title_node = Printf.sprintf "<title>%s</title>" title_string |> Soup.parse in
    let () = Soup.replace title_node new_title_node in
    Ok ()

(* This should better be a Map *)
let widgets = [("title", set_title)]
