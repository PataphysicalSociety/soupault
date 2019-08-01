(* Footnotes *)

(** Gets, or makes a unique id for a footnote "back link".

    If a footnote element already has an id, uses it,
    if not, uses "footnote-ref-$num".
 *)
let get_backlink_id el num =
  let el_id = Soup.attribute "id" el in
  match el_id with
  | Some el_id -> el_id
  | None -> Printf.sprintf "footnote-ref-%d" num

let rec move_footnotes link_class back_links ref_tmpl note_tmpl notes container num =
  match notes with
  | [] -> ()
  | n :: ns ->
    let open Soup.Infix in
    let num = num + 1 in
    let fn_id = Printf.sprintf "footnote-%d" num in
    let backlink_id = get_backlink_id n num in
    (* Create the footnote number element *)
    let note_ref =(Soup.parse ref_tmpl) $ "*" in
    let () = Soup.append_child note_ref (Soup.create_text (string_of_int num)) in
    let () = if back_links then Soup.set_attribute "id" backlink_id note_ref in
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
    let ref_text = string_of_int num in
    let ref_content =
      if back_links then
        let rc = Soup.create_element ~attributes:["href", "#" ^ backlink_id] ~inner_text:ref_text "a" in
        let () = Utils.add_class link_class rc in
        rc
     else Soup.create_element ~inner_text:ref_text "span"
    in
    let () = Soup.append_child note_ref' ref_content in
    (* Now insert the ref and the original children of the footnote element
       into the new footnote wrapper and delete the original *)
    let () = Soup.append_child note_wrapper note_ref' in
    let () = Soup.iter (Soup.append_child note_wrapper) (Soup.children n) in
    let () = Soup.delete n in
    move_footnotes link_class back_links ref_tmpl note_tmpl ns container num

let footnotes _ config soup =
  let bind = CCResult.(>>=) in
  let%m selector = Config.get_string_result "Missing required option \"selector\"" "selector" config in
  let note_selector = Config.get_strings_relaxed ~default:[".footnote"] "footnote_selector" config in
  let%m ref_tmpl = Config.get_string_default "<sup></sup>" "ref_template" config |> Utils.check_template "*" in
  let%m note_tmpl = Config.get_string_default "<p></p>" "footnote_template" config |> Utils.check_template "*" in
  let fn_link_class = Config.get_string "footnote_link_class" config in
  let back_links = Config.get_bool_default true "back_links" config in
  let container = Soup.select_one selector soup in
  match container with
  | None -> Ok ()
  | Some container ->
    let notes = Utils.select_all note_selector soup in
    Ok (move_footnotes fn_link_class back_links ref_tmpl note_tmpl notes container 0)

