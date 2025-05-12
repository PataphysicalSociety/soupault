open Common

let (let*) = Result.bind

(* Title *)
let set_title _ config _ page =
  let soup = page.element_tree in
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
  let valid_options = List.append Config.common_widget_options ["selector"; "default"; "prepend"; "append"; "force"; "keep"] in
  let () = Config.check_options valid_options config {|widget "title"|} in
  let selectors = Config.find_strings_or ~default:["h1"] config ["selector"] in
  let prepend = Config.find_string_or ~default:"" config ["prepend"] in
  let append = Config.find_string_or ~default:"" config ["append"] in
  let default_title = Config.find_string_or ~default:"" config ["default"] in
  let force = Config.find_bool_or ~default:false config ["force"] in
  let keep = Config.find_bool_or ~default:false config ["keep"] in
  (* Artificially insert a title element if force=true.
     Can be useful in HTML processor mode to add consistency to a bunch of
     handwritten pages. *)
  let () =
    if force then
    begin
      (* NB: lambdasoup always inserts a <head> in the whole document parsing mode,
          so a page is guaranteed to have one,
          unsafe unwrapping is fine *)
      let head = Soup.select_one "head" soup |> Option.get in
      let title_elem = Soup.create_element "title" in
      Soup.append_child head title_elem
    end
  in
  (* Now to setting the title *)
  let title_node = Soup.select_one "head title" soup in
  match title_node with
  | None ->
    Logs.debug @@ fun m -> m "Page does not have a <title> element, assuming you don't want to set it"
  | Some title_node ->
    if (not (Html_utils.is_empty title_node)) && keep then () else
    let (>>=) = Option.bind in
    let title_string =
      Html_utils.select_any_of selectors soup >>= Html_utils.get_element_text
        |> make_title_string default_title prepend append in
    (* XXX: Both Soup.create_text and Soup.create_element ~inner_text:... escape special characters
       instead of expanding entities, so "&mdash;" becomes "&amp;mdash", which is not what we want.
       Soup.parse expands them, which is why it's used here *)
    let new_title_node = Printf.sprintf "<title>%s</title>" title_string |> Soup.parse in
    Soup.replace title_node new_title_node
