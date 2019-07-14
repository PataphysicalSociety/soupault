(* Breadcrumbs *)

open Defaults

let make_breadcrumbs nav_path bc_tmpl_str prepend append between =
  let rec aux xs bc_soup acc_href =
    let open Soup.Infix in
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
  (* XXX: reusing a soup for append_child doesn't seem to work as of 0.6.3,
     nodes retain their context forever.
     this is why they are parsed into a new soup every time *)
  let () = Soup.append_root bc_soup (Soup.parse prepend) in
  let () = aux nav_path bc_soup "" in
  let () = Soup.append_root bc_soup (Soup.parse append) in
  bc_soup

let check_breadcrumb_template tmpl_str =
  let s = Soup.parse tmpl_str in
  let a = Soup.select_one "a" s in
  match a with
  | Some _ -> Ok ()
  | None -> Error (Printf.sprintf "No <a> elements in breadcrumb template \"%s\", nowhere to set the link target" tmpl_str)

let breadcrumbs env config soup =
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

