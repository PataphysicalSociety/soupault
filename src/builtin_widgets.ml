open Defaults
open Soup.Infix

(* Let Option monad be the default monad *)
let (>>=) = CCOpt.(>>=)

let get_required_element selector err soup =
  let e = Soup.select_one selector soup in
  match e with
  | Some e -> Ok e
  | None -> Error err


(** Generic widgets *)

(** Element insertion and deletion *)

(** Inserts an HTML snippet from the [html] config option
    into the first element that matches the [selector] *)
let insert_html _ config soup =
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
        let%m html_str = Config.get_string_result "Missing required option \"file\"" "html" config in
        let () = Soup.append_child container (Soup.parse html_str)
        in Ok ()
    end

(** Checks if a node is empty

    A node is considered empty iff it has no children but whitespace nodes.
    If a node has no children, it's clearly empty, and if it has more than one,
    then clearly isn't.
    The interesting case is one child. Sadly, lambdasoup has no function for checking
    node type now, so instead we check if its content is empty when converted
    to a string and stripped of whitespace.
 *)
let is_empty node =
  let open Soup in
  let children = children node in
  match (Soup.count children) with
  | 0 -> true
  | 1 -> (children |> first |> Utils.unwrap_option |> to_string |> String.trim) = ""
  | _ -> false

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
        if not (is_empty container) && when_empty then Ok ()
        else Ok (Soup.delete container)
    end

(* Reads a file specified in the [file] config option and inserts its content into the first element
   that matches the [selector] *)
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

(** Runs the [command] and inserts it output into the element that matches that [selector] *)
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

(* Title *)
let set_title _ config soup =
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
    let title_string =
      Soup.select_one selector soup >>= Soup.leaf_text |> make_title_string default_title prepend append in
    (* XXX: Both Soup.create_text and Soup.create_element ~inner_text:... escape special characters
       instead of expanding entities, so "&mdash;" becomes "&amp;mdash", which is not what we want.
       Soup.parse expands them, which is why it's used here *)
    let new_title_node = Printf.sprintf "<title>%s</title>" title_string |> Soup.parse in
    let () = Soup.replace title_node new_title_node in
    Ok ()

(* Breadcrumbs *)

let make_breadcrumbs nav_path bc_tmpl_str prepend append between =
  let rec aux xs bc_soup acc_href =
    match xs with
    | [] -> ()
    | x :: xs ->
      (* Create a fresh soup from the template so that we can mangle it without fear. *)
      let bc_tmpl = Soup.parse bc_tmpl_str in
      (* href for each next level accumulates, e.g. section, section/subsection... *)
      let acc_href = Printf.sprintf "%s/%s" acc_href x in
      (* Sanity checking is done by the widget wrapper,
         so here it's safe to use $ and other exception-throwing functions *)
      let bc_a = bc_tmpl $ "a" in
      let () = Soup.set_attribute "href" acc_href bc_a in
      (* x here is the section name *)
      let () = Soup.append_child bc_a (Soup.create_text x) in
      let () = Soup.append_root bc_soup bc_tmpl in
      (* Fixup: don't insert the "between" after the last element *)
      let () = if (List.length xs) >= 1 then Soup.append_root bc_soup (Soup.parse between) in
      aux xs bc_soup acc_href
  in
  let bc_soup = Soup.create_soup () in
  (* XXX: reusing a soup for append_child seem to work,
     this is why they are parsed into a new soup every time *)
  let () = Soup.append_root bc_soup (Soup.parse prepend) in
  let () = aux (List.rev nav_path) bc_soup "" in
  let () = Soup.append_root bc_soup (Soup.parse append) in
  bc_soup

let check_breadcrumb_template tmpl_str =
  let s = Soup.parse tmpl_str in
  let a = Soup.select_one "a" s in
  match a with
  | Some _ -> Ok ()
  | None -> Error (Printf.sprintf "No <a> elements in breadcrumb template \"%s\", nowhere to set the link target" tmpl_str)

let add_breadcrumbs env config soup =
  let min_depth = Config.get_int_default 1 "min_depth" config in
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
        let path_length = List.length env.nav_path in
        if path_length < min_depth then Ok () else
        let bc_tmpl_str = Config.get_string_default "<a></a>" "breadcrumb_template" config in
        let%m _  = check_breadcrumb_template bc_tmpl_str in
        let prepend = Config.get_string_default "" "prepend" config in
        let append = Config.get_string_default "" "append" config in
        let between = Config.get_string_default "" "between" config in
        let breadcrumbs = make_breadcrumbs env.nav_path bc_tmpl_str prepend append between in

        let () = Soup.append_child container breadcrumbs in Ok ()
    end

(* Footnotes *)
let rec _move_footnotes link_class ref_tmpl note_tmpl notes container num =
  match notes with
  | [] -> ()
  | n :: ns ->
    let open Soup.Infix in
    let num = num + 1 in
    let fn_id = Printf.sprintf "footnote-%d" num in
    (* Create the footnote number element *)
    let note_ref =(Soup.parse ref_tmpl) $ "*" in
    let () = Soup.append_child note_ref (Soup.create_text (string_of_int num)) in
    (* Create the footnote anchor that will be inserted in place of the original note *)
    let note_link = Soup.create_element ~attributes:["href", "#" ^ fn_id] "a" in
    let () = Soup.append_child note_link note_ref in
    let () = Utils.add_class link_class note_link in
    (* Insert the anchor before the original footnote element *)
    let () = Soup.insert_before n note_link in
    (* Create a wrapper for the footnote's new location *)
    let note_wrapper = (Soup.parse note_tmpl) $ "*" in
    let () = Soup.set_attribute "id" fn_id  note_wrapper in
    let () = Soup.append_child container note_wrapper in
    (* Reusing elements doesn't work and there's no node cloning yet, so we create a new one *)
    let note_ref' = (Soup.parse ref_tmpl) $ "*" in
    let () = Soup.append_child note_ref' (Soup.create_text (string_of_int num)) in
    (* Now insert the ref and the original children of the footnote element
       into the new footnote wrapper and delete the original *)
    let () = Soup.append_child note_wrapper note_ref' in
    let () = Soup.iter (Soup.append_child note_wrapper) (Soup.children n) in
    let () = Soup.delete n in
    _move_footnotes link_class ref_tmpl note_tmpl ns container num

let move_footnotes _ config soup =
  let bind = CCResult.(>>=) in
  let%m selector = Config.get_string_result "Missing required option \"selector\"" "selector" config in
  let%m note_selector = Config.get_string_result "Missing required option \"footnote_selector\"" "footnote_selector" config in
  let%m ref_tmpl = Config.get_string_default "<sup></sup>" "ref_template" config |> Utils.check_template "*" in
  let%m note_tmpl = Config.get_string_default "<p></p>" "footnote_template" config |> Utils.check_template "*" in
  let fn_link_class = Config.get_string "footnote_link_class" config in
  let container = Soup.select_one selector soup in
  match container with
  | None -> Ok ()
  | Some container ->
    let notes = Soup.select note_selector soup |> Soup.to_list in
    Ok (_move_footnotes fn_link_class ref_tmpl note_tmpl notes container 0)

(* This should better be a Map *)
let widgets = [
  ("include", include_file);
  ("insert_html", insert_html);
  ("delete_element", delete_element);
  ("exec", include_program_output);
  ("title", set_title);
  ("breadcrumbs", add_breadcrumbs);
  ("footnotes", move_footnotes)
]
