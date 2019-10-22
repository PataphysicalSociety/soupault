(* HTML tree manipulation widgets *)

(** Deletes an element from the tree *)
let delete_element _ config soup =
  let valid_options = List.append Config.common_widget_options ["selector"; "only_if_empty"] in
  let () = Config.check_options valid_options config "widget \"delete_element\"" in
  let selector = Config.get_string_result "Missing required option \"selector\"" "selector" config in
  let when_empty = Config.get_bool_default false "only_if_empty" config in
  match selector with
  | Error _ as e -> e
  | Ok selector ->
    let container = Soup.select_one selector soup in
    begin
      match container with
      | None ->
        let () = Logs.debug @@ fun m -> m "Page has no elements matching selector \"%s\", nothing to delete" selector in
        Ok ()
      | Some container ->
        if not (Utils.is_empty container) && when_empty then
          let () = Logs.debug @@ fun m -> m "Element matching selector \"%s\" is not empty, configured to delete only when empty" selector in
          Ok ()
        else Ok (Soup.delete container)
    end

