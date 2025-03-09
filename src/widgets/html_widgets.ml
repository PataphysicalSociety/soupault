(* HTML tree manipulation widgets *)

module OH = Otoml.Helpers

open Defaults
open Widget_utils

let (let*) = Stdlib.Result.bind

(** Deletes an element from the tree *)
let delete_element _ config _ page =
  let soup = page.element_tree in
  let valid_options = List.append Config.common_widget_options [
    "selector"; "only_if_empty"; "when_no_child"; "delete_all"
  ]
  in
  let () = Config.check_options valid_options config {|widget "delete_element"|} in
  let selector = Config.find_string_result config ["selector"] in
  let when_empty = Config.find_bool_or ~default:false config ["only_if_empty"] in
  let when_no_child = Otoml.Helpers.find_string_opt config ["when_no_child"] in
  let delete_all = Config.find_bool_or ~default:true config ["delete_all"] in
  match selector with
  | Error _ as e -> e
  | Ok selector ->
    let nodes =
      if delete_all then (Soup.select selector soup |> Soup.to_list)
      else (Soup.select_one selector soup |> (fun n -> match n with Some n -> [n] | None -> []))
    in
    begin
      match nodes with
      | [] ->
         Logs.debug @@ fun m -> m {|Page has no elements matching selector "%s", nothing to delete|} selector
      | ns ->
        let _delete when_empty when_no_child n =
          if not (Html_utils.is_empty n) && when_empty then
            Logs.debug @@ fun m -> m {|Element matching selector "%s" is not empty, not deleting it|} selector
          else
            begin
              match when_no_child with
              | None -> Soup.delete n
              | Some child_selector ->
                let child = Soup.select_one child_selector n in
                if Option.is_none child then Soup.delete n
                else Logs.debug @@ fun m -> m {|Element matching selector "%s" matches selector "%s", not deleting it|}
                  selector child_selector
            end
        in List.iter (_delete when_empty when_no_child) ns
    end;
    Ok ()

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
    let* () = Html_utils.wrap ~selector:s w_soup (Soup.to_string e |> Soup.parse) in
    let () = Soup.replace e w_soup in
    Ok ()
  in
  let valid_options = List.append Config.common_widget_options ["selector"; "wrapper"; "wrap_all"; "wrapper_selector"] in
  let () = Config.check_options valid_options config {|widget "wrap"|} in
  let selectors = get_selectors config in
  let wrapper_selector = OH.find_string_opt config ["wrapper_selector"] in
  let wrap_all = Config.find_bool_or ~default:true config ["wrap_all"] in
  match selectors with
  | Error _ as e -> e
  | Ok selectors ->
    let containers =
      if wrap_all then Html_utils.select_all selectors soup
      else (match Html_utils.select_any_of selectors soup with None -> [] | Some e -> [e])
    in
    begin
      match containers with
      | [] ->
        let () = no_container_action selectors "nothing to wrap" in Ok ()
      | _ ->
        let* wrapper_str = Config.find_string_result config ["wrapper"] in
        let* () = Utils.iter_result (wrap_elem wrapper_selector wrapper_str) containers in
        Ok ()
    end
