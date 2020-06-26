(* HTML tree manipulation widgets *)

(** Deletes an element from the tree *)
let delete_element _ config soup =
  let valid_options = List.append Config.common_widget_options ["selector"; "only_if_empty"; "delete_all"] in
  let () = Config.check_options valid_options config "widget \"delete_element\"" in
  let selector = Config.get_string_result "Missing required option \"selector\"" "selector" config in
  let when_empty = Config.get_bool_default false "only_if_empty" config in
  let delete_all = Config.get_bool_default true "delete_all" config in
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
         Logs.debug @@ fun m -> m "Page has no elements matching selector \"%s\", nothing to delete" selector
      | ns ->
        let _delete when_empty n =
          if not (Html_utils.is_empty n) && when_empty then
            Logs.debug @@ fun m -> m "Element matching selector \"%s\" is not empty, configured to delete only when empty" selector
          else Soup.delete n
        in List.iter (_delete when_empty) ns
    end;
    Ok ()

