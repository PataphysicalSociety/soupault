include Soupault_common

(** Parse HTML in specific context.

  HTML parser behaviour depends on context. For example, when the result is supposed to be
  a complete document, it must insert <head> or <body> elements if they are missing.
  If the result is supposed to be a fragment, it must not do that.

  lambdasoup doesn't provide a context-aware parsing function, so we turn to lower level
  Markup.ml for that.
 *)
let parse_html ?(body=true) str =
  let context = if body then `Fragment "body" else `Fragment "head" in
  Markup.string str |> Markup.parse_html ~context:context |> Markup.signals |> Soup.from_signals

(* Result-aware element selection functions *)
let wrap_select f selector soup =
  try Ok (f selector soup)
  with Soup.Parse_error msg -> Error (Printf.sprintf "Invalid CSS selector '%s', parse error: %s" selector msg)

let select selector soup = wrap_select Soup.select selector soup
let select_one selector soup = wrap_select Soup.select_one selector soup

(** Checks if a "template" has a specific element in it.
    For checking if there's any element at all, use "*" selector *)
let check_template selector template =
  let soup = Soup.parse template in
  let content_container = Soup.select_one selector soup in
  match content_container with
  | None -> Error (Printf.sprintf "Template %s has no element matching selector \"%s\"" template selector)
  | Some _ -> Ok template

(** Gets an element and returns Error if it doesn't exist,
    another bit of monadic convenience *)
let get_required_element selector err soup =
  let (let*) = Stdlib.Result.bind in
  let* e = select_one selector soup in
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
    let e =
      try Soup.select_one s soup
      with Soup.Parse_error msg -> Printf.ksprintf soupault_error "Invalid CSS selector '%s', parse error: %s" s msg
    in
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
        let nodes =
          try Soup.select s soup
          with Soup.Parse_error msg -> Printf.ksprintf soupault_error "Invalid CSS selector '%s', parse error: %s" s msg
        in
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
    let suggestion = (match suggestion with Some s -> (Printf.sprintf " Did you mean \"%s?\"" s) | None -> "") in
    let () = Logs.warn @@ fun m -> m "Invalid action \"%s\", using default (append child).%s" action suggestion in
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
      | None -> Error (Printf.sprintf "the wrapper does not have an element matching selector \"%s\"" s)
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
