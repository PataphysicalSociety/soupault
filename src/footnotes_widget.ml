(* Footnotes *)
let rec move_footnotes link_class ref_tmpl note_tmpl notes container num =
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
    move_footnotes link_class ref_tmpl note_tmpl ns container num

let footnotes _ config soup =
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
    Ok (move_footnotes fn_link_class ref_tmpl note_tmpl notes container 0)

