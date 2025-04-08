(* Footnotes *)

open Defaults
open Soupault_common

module OH = Otoml.Helpers

(** Makes a unique id for a footnote element--
    the footnote text that is taken out of the document body and
    moved to a designated footnotes container.

    If a footnote element already has an id, uses it,
    if not, uses "footnote-$num".

    If the prepend option is configured, prepends it to the id
    to create a footnotes "namespace", e.g. "fn-my-footnote".
 *)
let make_footnote_id ?(prepend="") el num =
  let el_id = Soup.attribute "id" el in
  match el_id with
  | Some el_id -> Printf.sprintf "%s%s" prepend el_id
  | None -> Printf.sprintf "footnote-%d" num

(** Makes a unique id for the footnote reference element
    (the one the original footnote text is replaced with)
    for jumping from the footnote back to the paragraph
    that refers to it.

    If a footnote element has an id, appends some text to it ("-ref" by default)
    to make it different from the footnote id.
    If the prepend option is configured, it also prepends that text
    to the id, e.g. "fn-my-footnote-ref".
 *)
let make_backlink_id ?(append="-ref") ?(prepend="") el num =
  let el_id = Soup.attribute "id" el in
  match el_id with
  | Some el_id -> Printf.sprintf "%s%s%s" prepend el_id append
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
      Html_utils.add_class link_class ref_content;
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
  let () = Soup.append_child fn_link fn_ref; Html_utils.add_class link_class fn_link in
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
let rec move_footnotes link_class back_links ref_tmpl note_tmpl notes container append prepend num =
  match notes with
  | [] -> ()
  | n :: ns ->
    let num = num + 1 in
    let fn_id = make_footnote_id ~prepend:prepend n num in
    let backlink_id = make_backlink_id ~append:append ~prepend:prepend n num in
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
      Soup.append_root container fn_wrapper;
      Soup.delete n
    in move_footnotes link_class back_links ref_tmpl note_tmpl ns container append prepend num

(** Footnotes widget wrapper *)
let footnotes _ config _ page =
  let soup = page.element_tree in
  let valid_options = List.append Config.common_widget_options
    ["selector"; "footnote_selector"; "ref_template"; "footnote_template"; "footnote_link_class";
     "back_links"; "back_link_id_append"; "link_id_prepend"; "action"] in
  let () = Config.check_options valid_options config {|widget "footnotes"|} in
  let selectors = Config.find_strings config ["selector"] in
  let action = OH.find_string_opt config ["action"] in
  let note_selector = Config.find_strings_or ~default:[".footnote"] config ["footnote_selector"] in
  let ref_tmpl =
    Config.find_string_or ~default:"<sup></sup>" config ["ref_template"] |>
    Html_utils.check_template widget_error "*"
  in
  let note_tmpl =
    Config.find_string_or ~default:"<p></p>" config ["footnote_template"] |>
    Html_utils.check_template widget_error "*"
  in
  let fn_link_class = OH.find_string_opt config ["footnote_link_class"] in
  let back_links = Config.find_bool_or ~default:true config ["back_links"] in
  let back_link_append = Config.find_string_or ~default:"-ref" config ["back_link_id_append"] in
  let link_prepend = Config.find_string_or ~default:"" config ["link_id_prepend"] in
  let container = Html_utils.select_any_of selectors soup in
  match container with
  | None ->
    Widget_utils.no_container_action selectors "nowhere to insert the footnotes"
  | Some container ->
    let notes = Html_utils.select_all note_selector soup in
    let container_content = Soup.create_soup () in
    let () = move_footnotes fn_link_class back_links ref_tmpl note_tmpl notes container_content back_link_append link_prepend 0 in
    Html_utils.insert_element action container (Soup.coerce container_content)
