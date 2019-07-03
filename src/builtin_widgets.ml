open Defaults

(* Let Option monad be the default monad *)
let (>>=) = CCOpt.(>>=)


(* Generic widgets *)

let include_file _ config soup =
  let selector = Config.get_string_result "Missing required option \"selector\"" "selector" config in
  match selector with
  | Error _ as e -> e
  | Ok selector ->
    let container = Soup.select_one selector soup in
    let bind = CCResult.(>>=) in
    begin
      match container with
      | None -> Ok ()
      | Some container ->
        let%m file = Config.get_string_result "Missing required option \"file\"" "file" config in
        let parse_content = Config.get_bool_default true "parse" config in
        let%m content = Utils.get_file_content file in
        let () =
          if parse_content then Soup.append_child container (Soup.parse content)
          else Soup.append_child container (Soup.create_text content)
        in Ok ()
    end

(* External program output inclusion *)

let make_program_env env =
  let make_var l r = Printf.sprintf "%s=%s" l r in
  let page_file = make_var "PAGE_FILE" env.page_file in
  [| page_file |]

let include_program_output env config soup =
  let selector = Config.get_string_result "Missing required option \"selector\"" "selector" config in
  match selector with
  | Error _ as e -> e
  | Ok selector ->
    let container = Soup.select_one selector soup in
    let bind = CCResult.(>>=) in
    begin
      match container with
      | None -> Ok ()
      | Some container ->
        let env_array = make_program_env env in
        let parse_content = Config.get_bool_default true "parse" config in
        let%m cmd = Config.get_string_result "Missing required option \"command\"" "command" config in
        let%m content = Utils.get_program_output cmd env_array in
        let () =
          if parse_content then Soup.append_child container (Soup.parse content)
          else Soup.append_child container (Soup.create_text content)
        in Ok ()
    end


(* High level widgets *)

let set_title _ config soup =
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
let widgets = [
  ("include", include_file);
  ("exec", include_program_output);
  ("title", set_title)
]
