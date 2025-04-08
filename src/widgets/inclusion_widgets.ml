open Defaults
open Soupault_common
open Widget_utils

let html_of_string ?(parse=true) ?(body_context=true) settings html_str =
  let context = if body_context then `Fragment "body" else `Fragment "head" in
  if parse then Html_utils.parse_html ~context:context ~encoding:settings.page_character_encoding html_str |> Soup.coerce
  else Soup.create_text html_str

(** Widgets that include external resources into the page *)

(** Inserts an HTML snippet from the [html] config option
    into the first element that matches the [selector] *)
let insert_html state config _ page =
  let soup = page.element_tree in
  let settings = state.soupault_settings in
  let valid_options = List.append Config.common_widget_options ["selector"; "html"; "parse"; "action"; "html_context_body"] in
  let () = Config.check_options valid_options config {|widget "insert_html"|} in
  let selectors = Config.find_strings config ["selector"] in
  let action = Otoml.Helpers.find_string_opt config ["action"] in
  let html_body_context = Config.find_bool_or ~default:true config ["html_context_body"] in
  let parse_content = Config.find_bool_or ~default:true config ["parse"] in
  let container = Html_utils.select_any_of selectors soup in
  begin match container with
  | None ->
    no_container_action selectors "nowhere to insert the snippet"
  | Some container ->
    let html_str = Config.find_string config ["html"] in
    let content = html_of_string ~parse:parse_content ~body_context:html_body_context settings html_str in
    Html_utils.insert_element action container content
  end

(* Reads a file specified in the [file] config option and inserts its content into the first element
   that matches the [selector] *)
let include_file state config _ page =
  let soup = page.element_tree in
  let settings = state.soupault_settings in
  let valid_options = List.append Config.common_widget_options ["selector"; "file"; "parse"; "action"; "html_context_body"] in
  let () = Config.check_options valid_options config {|widget "include"|} in
  let selectors = Config.find_strings config ["selector"] in
  let action = Otoml.Helpers.find_string_opt config ["action"] in
  let html_body_context = Config.find_bool_or ~default:true config ["html_context_body"] in
  let parse_content = Config.find_bool_or ~default:true config ["parse"] in
  let container = Html_utils.select_any_of selectors soup in
  begin match container with
  | None ->
    no_container_action selectors "nowhere to insert the file contents"
  | Some container ->
    let file = Config.find_string config ["file"] in
    let content =
      (try Soup.read_file file
      with Sys_error msg -> widget_error msg)
    in
    let content = html_of_string ~parse:parse_content ~body_context:html_body_context settings content in
    Html_utils.insert_element action container content
  end

(* External program output inclusion *)

let make_program_env page =
  let make_var l r = Printf.sprintf "%s=%s" l r in
  let page_file = make_var "PAGE_FILE" page.page_file in
  let page_url = make_var "PAGE_URL" page.url in
  let target_dir = make_var "TARGET_DIR" page.target_dir in
  [| page_file; page_url; target_dir |]

(** Runs the [command] and inserts it output into the element that matches that [selector] *)
let include_program_output state config _ page =
  let soup = page.element_tree in
  let settings = state.soupault_settings in
  let valid_options = List.append Config.common_widget_options ["selector"; "command"; "parse"; "action"; "html_context_body"] in
  let () = Config.check_options valid_options config {|widget "exec"|} in
  let selectors = Config.find_strings config ["selector"] in
  let action = Otoml.Helpers.find_string_opt config ["action"] in
  let html_body_context = Config.find_bool_or ~default:true config ["html_context_body"] in
  let parse_content = Config.find_bool_or ~default:true config ["parse"] in
  let container = Html_utils.select_any_of selectors soup in
  match container with
  | None ->
    no_container_action selectors "nowhere to insert the program output"
  | Some container ->
    let env_array = make_program_env page in
    let cmd = Config.find_string config ["command"] in
    let content = Process_utils.get_program_output ~env:env_array ~debug:settings.debug cmd in
    begin match content with
    | Ok content ->
      let content_etree = html_of_string ~parse:parse_content ~body_context:html_body_context settings content in
      Html_utils.insert_element action container content_etree
    | Error msg -> widget_error msg
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
let preprocess_element state config _ page =
  let soup = page.element_tree in
  let settings = state.soupault_settings in
  let run_command page command action parse body_context node =
    let input = Html_utils.inner_html ~escape_html:false node in
    let cached_result = Cache.get_cached_object settings page.page_file command input in
    match cached_result with
    | Some output ->
      let () = Logs.info @@ fun m -> m {|The result of executing command "%s" was found in cache|} command in
      let content = html_of_string ~parse:parse ~body_context:body_context settings output in
      Html_utils.insert_element action node content
    | None ->
      let () = Logs.info @@ fun m -> m "Executing command: %s" command in
      let node_env = make_node_env node in
      let program_env = make_program_env page in
      let env_array = Array.append program_env node_env in
      let result = Process_utils.get_program_output ~input:(Some input) ~env:env_array ~debug:settings.debug command in
      begin
        match result with
        | Ok output ->
          let () = Cache.cache_object settings page.page_file command input output in
          let content = html_of_string ~parse:parse ~body_context:body_context settings output in
          Html_utils.insert_element action node content
        | Error msg -> widget_error msg
      end
  in
  (* Retrieve configuration options *)
  let valid_options = List.append Config.common_widget_options ["selector"; "command"; "parse"; "action"; "html_context_body"] in
  let () = Config.check_options valid_options config {|widget "preprocess_element"|} in
  (* This widget replaces the original element with its preprocessed version by default. *)
  let action = Config.find_string_or ~default:"replace_content" config ["action"] in
  let parse = Config.find_bool_or ~default:true config ["parse"]  in
  let html_body_context = Config.find_bool_or ~default:true config ["html_context_body"] in
  let selectors = Config.find_strings config ["selector"] in
  let command = Config.find_string config ["command"] in
  let nodes = Html_utils.select_all selectors soup in
  List.iter (run_command page command (Some action) parse html_body_context) nodes
