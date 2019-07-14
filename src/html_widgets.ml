(* HTML tree manipulation widgets *)

(** Deletes an element from the tree *)
let delete_element _ config soup =
  let selector = Config.get_string_result "Missing required option \"selector\"" "selector" config in
  let when_empty = Config.get_bool_default false "only_if_empty" config in
  match selector with
  | Error _ as e -> e
  | Ok selector ->
    let container = Soup.select_one selector soup in
    begin
      match container with
      | None -> Ok ()
      | Some container ->
        if not (Utils.is_empty container) && when_empty then Ok ()
        else Ok (Soup.delete container)
    end

