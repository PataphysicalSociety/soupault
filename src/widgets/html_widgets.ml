(* HTML tree manipulation widgets *)

module OH = Otoml.Helpers

open Common

(** Deletes an element from the tree *)
let delete_element _ config _ page =
  let soup = page.element_tree in
  let valid_options = List.append Config.common_widget_options [
    "selector"; "only_if_empty"; "when_no_child"; "delete_all"
  ]
  in
  let () = Config.check_options valid_options config {|widget "delete_element"|} in
  let selectors = Config.find_strings config ["selector"] in
  let when_empty = Config.find_bool_or ~default:false config ["only_if_empty"] in
  let when_no_child = Otoml.Helpers.find_string_opt config ["when_no_child"] in
  let delete_all = Config.find_bool_or ~default:true config ["delete_all"] in
  let nodes =
    if delete_all then Html_utils.select_all selectors soup
    else (Html_utils.select_any_of selectors soup |> (fun n -> match n with Some n -> [n] | None -> []))
  in
  begin match nodes with
  | [] -> Utils.no_container_action selectors "nothing to delete"
  | ns ->
    let _delete when_empty when_no_child n =
      if not (Html_utils.is_empty n) && when_empty then
        Logs.debug @@ fun m -> m {|Element matching a selector from %s is not empty, not deleting it|}
          (Utils.format_list ~quote:false selectors)
      else
        begin match when_no_child with
        | None -> Soup.delete n
        | Some child_selector ->
          let child = Soup.select_one child_selector n in
          if Option.is_none child then Soup.delete n
          else Logs.debug @@ fun m -> m "Element that maches a selector from %s\
            has a child that matches selector \"%s\", not deleting it"
            (Utils.format_list ~quote:false selectors) child_selector
        end
      in List.iter (_delete when_empty when_no_child) ns
    end

(** Wraps elements matching certain selectors into an HTML snippet. *)
let wrap _ config _ page =
  let soup = page.element_tree in
  let wrap_elem s w e =
    let w_soup = Soup.parse w in
    (* XXX: This is a rather inelegant dirty hack.
       Since the wrapper comes from outside the page element tree,
       it's not enough to insert elements into it, we also need to get it into the original tree.
       Since we can only insert the wrapped element when we know wrapping worked fine,
       we can't get the wrapper into the tree early.
       That's why we first "clone" the "wrappee" by exporting it to string and parsing it again,
       then create a separate tree from the wrapper and the wrappee,
       and finally replace the original element with it.

       If this turns out to have undesirable side effects,
       we'll need to search for better hacks. *)
    let res = Html_utils.wrap ~selector:s w_soup (Soup.to_string e |> Soup.parse) in
    match res with
    | Ok () -> Soup.replace e w_soup
    | Error msg -> widget_error msg
  in
  let valid_options = List.append Config.common_widget_options ["selector"; "wrapper"; "wrap_all"; "wrapper_selector"] in
  let () = Config.check_options valid_options config {|widget "wrap"|} in
  let selectors = Config.find_strings config ["selector"] in
  let wrapper_selector = OH.find_string_opt config ["wrapper_selector"] in
  let wrap_all = Config.find_bool_or ~default:true config ["wrap_all"] in
  let containers =
    if wrap_all then Html_utils.select_all selectors soup
    else (match Html_utils.select_any_of selectors soup with None -> [] | Some e -> [e])
  in
  begin match containers with
  | [] ->
    Utils.no_container_action selectors "nothing to wrap"
  | _ ->
    let wrapper_str = Config.find_string config ["wrapper"] in
    List.iter (wrap_elem wrapper_selector wrapper_str) containers
  end

(* Renders a template using attributes and content of an element
   and replaces the original element with the rendered template.
 *)
let element_template _ config _ page =
  let transform_element content_key template elem =
    (* Create an environment with element attributes and content. *)
    let attrs = Soup.fold_attributes
      (fun acc name value -> (name, Jingoo.Jg_types.box_string value) :: acc) [] elem
    in
    let inner_html = Html_utils.inner_html elem in
    (* The content key is configurable. *)
    let env = (content_key, Jingoo.Jg_types.box_string inner_html) :: attrs in
    let res = Template.render template env in
    let new_soup = Soup.parse res in
    Soup.replace elem new_soup
  in
  let soup = page.element_tree in
  let valid_options = List.append Config.common_widget_options [
    (* Selector(s) of elements that are supposed to be transformed
       using a template.
     *)
    "selector";

    (* Template source code. *)
    "template";

    (* The template environment key associated with the element content.
       It's configurable in case someone wants to use [content]
       as an attribute name.
     *)
    "content_key";
  ]
  in
  let () = Config.check_options valid_options config {|widget "element_template"|} in
  let selectors = Config.find_strings config ["selector"] in
  let content_key = Config.find_string_or ~default:"content" config ["content_key"] in
  let template = Config.find_string config ["template"] |> Template.of_string in
  let elems = Html_utils.select_all selectors soup in
  List.iter (transform_element content_key template) elems
