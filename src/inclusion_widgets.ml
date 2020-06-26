open Defaults

let (let*) = Stdlib.Result.bind

(** Widgets that include external resources into the page *)

(** Inserts an HTML snippet from the [html] config option
    into the first element that matches the [selector] *)
let insert_html _ config soup =
  let valid_options = List.append Config.common_widget_options ["selector"; "html"; "action"] in
  let () = Config.check_options valid_options config "widget \"insert_html\"" in
  let selector = Config.get_string_result "Missing required option \"selector\"" "selector" config in
  let action = Config.get_string_default "append_child" "action" config in
  let html_body_context = Config.get_bool_default true "html_context_body" config in
  match selector with
  | Error _ as e -> e
  | Ok selector ->
    let* container = Html_utils.select_one selector soup in
    begin
      match container with
      | None ->
        let () = Logs.debug @@ fun m -> m "Page has no elements matching selector \"%s\", nowhere to insert the snippet" selector in
        Ok ()
      | Some container ->
        let* html_str = Config.get_string_result "Missing required option \"html\"" "html" config in
        let content = Html_utils.parse_html ~body:html_body_context html_str in
        Ok (Html_utils.insert_element action container content)
    end

(* Reads a file specified in the [file] config option and inserts its content into the first element
   that matches the [selector] *)
let include_file _ config soup =
  let valid_options = List.append Config.common_widget_options ["selector"; "file"; "parse"; "action"; "html_context_body"] in
  let () = Config.check_options valid_options config "widget \"include\"" in
  let selector = Config.get_string_result "Missing required option \"selector\"" "selector" config in
  let action = Config.get_string_default "append_child" "action" config in
  let html_body_context = Config.get_bool_default true "html_context_body" config in
  let parse_content = Config.get_bool_default true "parse" config in
  match selector with
  | Error _ as e -> e
  | Ok selector ->
    let* container = Html_utils.select_one selector soup in
    begin
      match container with
      | None ->
        let () = Logs.debug @@ fun m -> m "Page has no elements matching selector \"%s\", nowhere to insert the file" selector in
        Ok ()
      | Some container ->
        let* file = Config.get_string_result "Missing required option \"file\"" "file" config in
        let* content = Utils.get_file_content file in
        let content =
          if parse_content then (Html_utils.parse_html ~body:html_body_context content |> Soup.coerce)
          else Soup.create_text content
        in Ok (Html_utils.insert_element action container content)
    end

(* External program output inclusion *)

let make_program_env env =
  let make_var l r = Printf.sprintf "%s=%s" l r in
  let page_file = make_var "PAGE_FILE" env.page_file in
  let page_url = make_var "PAGE_URL" env.page_url in
  let target_dir = make_var "TARGET_DIR" env.target_dir in
  [| page_file; page_url; target_dir |]

(** Runs the [command] and inserts it output into the element that matches that [selector] *)
let include_program_output env config soup =
  let valid_options = List.append Config.common_widget_options ["selector"; "command"; "parse"; "action"] in
  let () = Config.check_options valid_options config "widget \"exec\"" in
  let selector = Config.get_string_result "Missing required option \"selector\"" "selector" config in
  let action = Config.get_string_default "append_child" "action" config in
  let html_body_context = Config.get_bool_default true "html_context_body" config in
  let parse_content = Config.get_bool_default true "parse" config in
  match selector with
  | Error _ as e -> e
  | Ok selector ->
    let* container = Html_utils.select_one selector soup in
    begin
      match container with
      | None ->
        let () = Logs.debug @@ fun m -> m "Page has no elements matching selector \"%s\", nowhere to insert the script output" selector in
        Ok ()
      | Some container ->
        let env_array = make_program_env env in
        let* cmd = Config.get_string_result "Missing required option \"command\"" "command" config in
        let* content = Utils.get_program_output cmd env_array in
        let content =
          if parse_content then (Html_utils.parse_html ~body:html_body_context content |> Soup.coerce)
          else Soup.create_text content
        in Ok (Html_utils.insert_element action container content)
    end

let make_node_env node =
  let make_var l r = Printf.sprintf "%s=%s" l r in
  let make_attr n v = make_var ("ATTR_" ^ String.uppercase_ascii n) v in
  let name = make_var "TAG_NAME" (Soup.name node) in
  let attrs = Soup.fold_attributes (fun acc n v -> make_attr n v :: acc) [ ] node in
  name :: attrs |> Array.of_list

(** Runs the [command] using the text of the element that matches the
 * specified [selector] as stdin. Reads stdout and replaces the content
 * of the element.*)
let preprocess_element env config soup =
  let run_command command action parse body_context node =
    let input = Some (Html_utils.inner_html ~escape_html:false node) in
    let () = Logs.info @@ fun m -> m "Executing command: %s" command in
    let node_env = make_node_env node in
    let program_env = make_program_env env in
    let env_array = Array.append program_env node_env in
    let result = Utils.get_program_output ~input:input command env_array in
    match result with
    | Ok text ->
        let content =
          if parse then Html_utils.parse_html ~body:body_context text |> Soup.coerce
          else Soup.create_text text
        in Html_utils.insert_element action node content
    | Error e ->
        raise (Failure e)
  in
  (* Retrieve configuration options *)
  let valid_options = List.append Config.common_widget_options ["selector"; "command"; "parse"; "action"] in
  let () = Config.check_options valid_options config "widget \"preprocess_element\"" in
  let action = Config.get_string_default "replace_content" "action" config in
  let parse = Config.get_bool_default true "parse" config in
  let html_body_context = Config.get_bool_default true "html_context_body" config in
  let* selectors =
    Config.get_strings_relaxed "selector" config |>
    (function [] -> Error "Missing required option \"selector\"" | _ as ss -> Ok ss)
  in
  let* command = Config.get_string_result "Missing required option \"command\"" "command" config in
  let nodes = Html_utils.select_all selectors soup in
  try
    Ok (List.iter (run_command command action parse html_body_context) nodes)
  with Failure e -> Error e
