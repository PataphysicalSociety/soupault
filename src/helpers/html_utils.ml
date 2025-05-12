open Common

(** Checks is a CSS selector is valid.

  Lambdasoup does not have a built-in option for that purpose,
  so we fake it by trying to select from an empty element tree
  with a given selector.
 *)
let check_selector opt_name s =
  let soup = Soup.create_soup () in
  try
    let _ = Soup.select_one s soup in
    Ok ()
  with Soup.Parse_error msg ->
    let msg = Printf.sprintf {|invalid CSS selector "%s" in option %s, parse error: %s|}
      s opt_name msg in
    Error msg

let rec check_selectors opt_name ss =
  match ss with
  | [] -> Ok ()
  | s :: ss' ->
    begin match (check_selector opt_name s) with
    | Ok () -> check_selectors opt_name ss'
    | (Error _ as e) -> e
    end

(** Parse HTML in specific context.

  HTML parser behaviour depends on context. For example, when the result is supposed to be
  a complete document, it must insert <head> or <body> elements if they are missing.
  If the result is supposed to be a fragment, it must not do that.

  lambdasoup doesn't provide a context-aware parsing function, so we turn to lower level
  Markup.ml for that.
 *)
let parse_html ?context ?(encoding=Markup.Encoding.utf_8) str =
  Markup.string str |> Markup.parse_html ?context:context ~encoding:encoding |> Markup.signals |> Soup.from_signals

(* An equivalent of [Soup.parse], but encoding-aware. *)
let parse_html_default ?(encoding=Markup.Encoding.utf_8) str =
  Markup.string str |> Markup.parse_html ~encoding:encoding |> Markup.signals |> Soup.from_signals

(* A high-level page parsing function. *)
let parse_page ?(fragment=true) settings page_source =
  (* As of lambdasoup 1.0.0, [Soup.parse] never fails, only returns empty element trees,
     so there's no need to handle errors here.

     First we use the default HTML parsing function (equivalent to [Soup.parse]) to get the element tree
     without any top-level structure corrections
     and see if it's intended to be a complete page rather than a fragment
     that the user may want to insert in a template.

     The problem with using that function for real parsing is that, as of 1.0.0,
     [Soup.parse] resolves certain ambiguities in a way that interferes with our templating:
     for example, it may insert a [<body>] tag if it sees tags normally found in [<head>], like [<style>],
     even though they are not prohibited in [<body>].

     Passing [~context:(`Fragment "body")] to [Markup.parse_html] solves that problem
     but creates a different problem instead: that strips top-level [<html>] tags
     from the document if they are present.

     So, for now at least, we parse HTML twice: first to determine if it's complete or partial,
     then to produce an actual element tree.
   *)
  let _interim_html = parse_html_default ~encoding:settings.page_character_encoding page_source in
  let html_elem = Soup.select_one "html" _interim_html in
  match html_elem with
  | Some _ ->
    (* If there's the ([<html>]) in the page, it's a complete page.
       We must not attempt to parse it as a fragment.
     *)
    parse_html ~context:`Document ~encoding:settings.page_character_encoding page_source
  | None ->
    (* If there's no [<html>] element, it's a partial page.
       We can parse it as a <body> fragment or a complete page, depending on the settings.
     *)
    let context = if fragment then (`Fragment "body") else `Document in
    parse_html ~context:context ~encoding:settings.page_character_encoding page_source

(** Checks if a "template" has a specific element in it.
    For checking if there's any element at all, use "*" selector *)
let check_template err_func selector template =
  let soup = Soup.parse template in
  let content_container = Soup.select_one selector soup in
  match content_container with
  | None ->
    err_func @@ Printf.sprintf
      {|Template "%s" has no element matching selector "%s"|} template selector
  | Some _ -> template

(** Gets an element and returns Error if it doesn't exist,
    another bit of monadic convenience *)
let get_required_element selector err soup =
  let e = Soup.select_one selector soup in
  match e with
  | Some e -> Ok e
  | None -> Error err

(** Adds class to an element if class is given *)
let add_class c e =
  match c with
  | Some c -> Soup.add_class c e
  | None -> ()

(** Extracts all text nodes from an element and its children if there're any,
    and returns them all as a single string.
    Essentially, strips HTML tags from element's inner HTML. *)
let get_element_text e =
  let texts = Soup.texts e in
  match texts with
  | [] -> None
  | _ ->
    (* "Normalize" the whitespace *)
    let text = String.concat "" texts |> String.trim in
    if text = "" then None else Some text

(** Select the first element matching any of given selectors *)
let rec select_any_of selectors soup =
  match selectors with
  | [] -> None
  | s :: ss ->
    let e = Soup.select_one s soup in
    begin
      match e with
      | Some _ as e -> e
      | None -> select_any_of ss soup
    end

(* Select all elements matching any of given selectors *)
let select_all selectors soup =
  let rec aux selectors soup acc =
    match selectors with
    | [] -> acc
    | s :: ss ->
        let nodes = Soup.select s soup in
	let acc = List.append (Soup.to_list nodes) acc in
        aux ss soup acc
  in aux selectors soup []

(* Checks if element matches any of given selectors. *)
let rec matches_any_of selectors soup elem =
  match selectors with
  | [] -> false
  | s :: ss ->
    if Soup.matches_selector soup s elem then true
    else matches_any_of ss soup elem

(** Creates a soup with child nodes of an element ripped out of their context *)
let child_nodes e =
  (* XXX: Rendering and re-parsing seems to be the only way to
     rip nodes out of their parent context now,
     and thus make them safe for adding to a different place *)
  let cs = Soup.children e |> Soup.to_list |> List.map (fun e -> Soup.to_string e |> Soup.parse) in
  let soup = Soup.create_soup () in
  let () = List.iter (Soup.append_root soup) cs in
  soup

(** Retrieves the innerHTML of an element --
    a string representation of its children *)
let inner_html ?(escape_html=true) e =
  let children = child_nodes e in
  if escape_html then Soup.to_string children
  else
      let escape_text = fun (x:string) -> x in
      let escape_attribute = fun (x:string) -> x in
      children |> Soup.signals |> (fun s -> Markup.write_html ~escape_text ~escape_attribute s) |> Markup.to_string

(** Appends a child if child rather than None is given *)
let append_child container child =
  match child with
  | None -> ()
  | Some c -> Soup.append_child container c

(** Replaces all content of a container node with something else *)
let replace_content container content =
  let () = Soup.iter Soup.delete (Soup.children container) in
  Soup.append_child container content

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
  | 1 -> (children |> first |> Option.get |> to_string |> String.trim) = ""
  | _ -> false

(** Inserts a node into the page at desired position *)
let insert_element action container content =
  let actions =
    [("append_child", Soup.append_child); ("prepend_child", Soup.prepend_child);
     ("insert_before", Soup.insert_before); ("insert_after", Soup.insert_after);
     ("replace_element", Soup.replace); ("replace_content", replace_content);
     ("ignore_output", fun _ _ -> ())]
  in
  let action = Option.value ~default:"append_child" action in
  let action_fun = List.assoc_opt action actions in
  match action_fun with
  | Some f -> f container content
  | None ->
    let index = Spellcheck.make_index (CCList.Assoc.keys actions) in
    let suggestion = Spellcheck.get_suggestion index action in
    let suggestion = (match suggestion with Some s -> (Printf.sprintf {| Did you mean "%s?"|} s) | None -> "") in
    let () = Logs.warn @@ fun m -> m {|Invalid action "%s", using default (append child).%s|} action suggestion in
    Soup.append_child container content

(* Wraps one HTML (sub)tree in another *)
let wrap ?(selector=None) wrapper content =
  let (let*) = Stdlib.Result.bind in
  (* The wrapper is assumed to be a parsed HTML snippet. We need to find an _element_ inside it
     that we can wrap something in. *)
  let get_wrapper_element wrapper =
    (* If the user passed an element node, we can just use it directly. *)
    if Soup.is_element wrapper then begin
      let we_opt = Soup.element wrapper in
      (* This error should never occur, but we are still unsafely unwrapping an option... *)
      let we = CCOption.get_exn_or "Soup.element returned None for an element node. Please report a bug." we_opt in
      Ok we
    end
    (* If we are given a parsed snippet, then we have to look inside it and search for elements. *)
    else match selector with
    | None ->
      (* Soup.select only selects elements, so we need not worry about non-element children *)
      let children = Soup.select "*" wrapper |> Soup.to_list in begin
      match children with
      | [] -> Error "the wrapper does not contain any element nodes, cannot wrap anything in it"
      | [c] ->
        Ok c
      | _ -> Error "the wrapper has more then one child element but the wrapper selector is not specified"
      end
    | Some s ->
      (* Right now the agreement is to always insert in the first matching elements if there are more than one *)
      let child = Soup.select_one s wrapper in begin
      match child with
      | None -> Error (Printf.sprintf {|the wrapper does not have an element matching selector "%s"|} s)
      | Some c -> Ok c
      end
  in
  let* we = get_wrapper_element wrapper in
  Ok (Soup.append_child we content)

(* Checks if an element is a heading *)
let is_heading e =
  match (Soup.name e) with
  | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" -> true
  | _ -> false

(* Returns the number from <h1> etc. *)
let get_heading_level e = String.sub (Soup.name e) 1 1 |> int_of_string

let find_headings soup =
  let open Soup in
  soup |> descendants |> elements |> filter is_heading |> to_list
