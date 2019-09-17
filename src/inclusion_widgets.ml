open Defaults

(** Widgets that include external resources into the page *)

(** Inserts an HTML snippet from the [html] config option
    into the first element that matches the [selector] *)
let insert_html _ config soup =
  let valid_options = List.append Config.common_widget_options ["selector"; "html"] in
  let () = Config.check_options valid_options config "widget \"insert_html\"" in
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
        let%bind html_str = Config.get_string_result "Missing required option \"html\"" "html" config in
        let () = Soup.append_child container (Soup.parse html_str)
        in Ok ()
    end

(* Reads a file specified in the [file] config option and inserts its content into the first element
   that matches the [selector] *)
let include_file _ config soup =
  let valid_options = List.append Config.common_widget_options ["selector"; "file"; "parse"] in
  let () = Config.check_options valid_options config "widget \"include\"" in
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
        let%bind file = Config.get_string_result "Missing required option \"file\"" "file" config in
        let parse_content = Config.get_bool_default true "parse" config in
        let%bind content = Utils.get_file_content file in
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

(** Runs the [command] and inserts it output into the element that matches that [selector] *)
let include_program_output env config soup =
  let valid_options = List.append Config.common_widget_options ["selector"; "command"; "parse"] in
  let () = Config.check_options valid_options config "widget \"exec\"" in
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
        let%bind cmd = Config.get_string_result "Missing required option \"command\"" "command" config in
        let%bind content = Utils.get_program_output cmd env_array in
        let () =
          if parse_content then Soup.append_child container (Soup.parse content)
          else Soup.append_child container (Soup.create_text content)
        in Ok ()
    end

