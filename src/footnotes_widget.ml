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

(** Creates a footnote reference number element --
    what appears in front of the footnote at the end of the document.

    If back_links option is true, it wraps the number in a link
    back to the original location.
 *)
let make_footnote_ref back_links backlink_id link_class ref_tmpl num =
  let open Soup.Infix in
  let ref_text = string_of_int num in
  let fn_ref = (Soup.parse ref_tmpl) $ "*" in
  let () =
    if back_links then
      let ref_content =
        Soup.create_element ~attributes:["href", "#" ^ backlink_id] ~inner_text:ref_text "a"
      in
      Utils.add_class link_class ref_content;
      Soup.append_child fn_ref ref_content
    else string_of_int num |> Soup.create_text |> Soup.append_child fn_ref
  in fn_ref

(** Creates the footnote link that will replace the original
    footnote element in the document *)
let make_footnote_link back_links backlink_id link_class ref_tmpl fn_id num =
  let open Soup.Infix in
  let fn_ref =(Soup.parse ref_tmpl) $ "*" in
  let () =
    Soup.create_text (string_of_int num) |> Soup.append_child fn_ref;
    if back_links then Soup.set_attribute "id" backlink_id fn_ref
  in
  let fn_link = Soup.create_element ~attributes:["href", "#" ^ fn_id] "a" in
  let () = Soup.append_child fn_link fn_ref; Utils.add_class link_class fn_link in
  fn_link

(* Creates a container for the footnote content
   that appears at the end of the document *)
let make_footnote_wrapper note_tmpl fn_id =
  let open Soup.Infix in
  let note_wrapper = (Soup.parse note_tmpl) $ "*" in
  let () = Soup.set_attribute "id" fn_id  note_wrapper in
  note_wrapper

(** Moves footnotes from the document text to a container element
    and replaces them with links *)
let rec move_footnotes link_class back_links ref_tmpl note_tmpl notes container num =
  match notes with
  | [] -> ()
  | n :: ns ->
    let num = num + 1 in
    let fn_id = Printf.sprintf "footnote-%d" num in
    let backlink_id = get_backlink_id n num in
    (* Create the footnote link that will replace the original footnote element *)
    let fn_link = make_footnote_link back_links backlink_id link_class ref_tmpl fn_id num in
    (* Insert the anchor before the original footnote element *)
    let () = Soup.insert_before n fn_link in
    (* Create a wrapper for the footnote's new location *)
    let fn_wrapper = make_footnote_wrapper note_tmpl fn_id in
    (* Create a reference number that will appear in front of the footnote *)
    let fn_ref = make_footnote_ref back_links backlink_id link_class ref_tmpl num in
    (* Now insert the reference and the original children of the footnote element
       into the new footnote wrapper and delete the original *)
    let () =
      Soup.append_child fn_wrapper fn_ref;
      Soup.iter (Soup.append_child fn_wrapper) (Soup.children n);
      Soup.append_child container fn_wrapper;
      Soup.delete n
    in move_footnotes link_class back_links ref_tmpl note_tmpl ns container num

(** Footnotes widget wrapper *)
let footnotes _ config soup =
  let bind = CCResult.(>>=) in
  let%bind selector = Config.get_string_result "Missing required option \"selector\"" "selector" config in
  let note_selector = Config.get_strings_relaxed ~default:[".footnote"] "footnote_selector" config in
  let%bind ref_tmpl = Config.get_string_default "<sup></sup>" "ref_template" config |> Utils.check_template "*" in
  let%bind note_tmpl = Config.get_string_default "<p></p>" "footnote_template" config |> Utils.check_template "*" in
  let fn_link_class = Config.get_string "footnote_link_class" config in
  let back_links = Config.get_bool_default true "back_links" config in
  let container = Soup.select_one selector soup in
  match container with
  | None -> Ok ()
  | Some container ->
    let notes = Utils.select_all note_selector soup in
    Ok (move_footnotes fn_link_class back_links ref_tmpl note_tmpl notes container 0)

