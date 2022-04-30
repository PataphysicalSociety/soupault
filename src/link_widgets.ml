(* Link target manipulation widgets *)

module OH = Otoml.Helpers

open Soupault_common

(* By default, exclude these categories of links from target rewriting:
     1. Links that have a URI schema (^([a-zA-Z0-9]+):), e.g. https://example.com
     2. Links to anchors within the same page (^#), e.g. #my-anchor
     3. Hand-made relative links (^\.), e.g. ../style.css
     4. Protocol-relative URLs that begin with //
 *)
let default_exclude_regex = "^((([a-zA-Z0-9]+):)|#|\\.|//)"

(* Exhaustive list of elements supported by link widgets. *)
let link_selectors = ["a"; "link"; "img"; "script"; "audio"; "video"; "object"; "embed"]

(** Updates attribute's value using a target rewriting function:
    relativize_link_target, absolutize_link_target, or a wrapper for one of those.

    This is a generic function that, by itself, is good for use with attributes
    that hold a single target, like "href" or "src".
    In that case it can use relativize/absolutize_link_target functions directly
    because they handle a single target.

    For use with attributes that may have multiple targets, like "srcset",
    it needs a wrapper that rewrites individual targets in the attribute value
    using relativize/absolutize_link_target functions and reassembles
    the attribute value with updated targets.
  *)
let process_attr func attr_name elem =
  let attr_value = Soup.attribute attr_name elem in
  match attr_value with
  | None ->
    (* No such attribute in the element, so there's nothing to do. *)
    ()
  | Some attr_value ->
    let new_attr_value = func attr_value in
    Soup.set_attribute attr_name new_attr_value elem

(* HTML uses attributes with wildly different names for link targets.
   Some elements may allow more than one possible attribute,
   and some attributes may have multiple targets in them.
   We have to maintain a map of element names to their allowed attributes
   plus functions that can be used to process them.
 *)
let attribute_map = [
  "a",      ["href", process_attr];
  "link",   ["href", process_attr];
  "script", ["src", process_attr];
  "audio",  ["src", process_attr];
  "video",  ["src", process_attr];
  "embed",  ["src", process_attr];
  "object", ["data", process_attr];
  "img",    ["src", process_attr];
]

(** Iterates through all relevant attributes of an element
    and applies appropriate attribute processing functions to them,
    according to the [attribute_map].
  *)
let process_attrs attr_processor elem =
  let tag_name = Soup.name elem in
  let mappings = List.assoc tag_name attribute_map in
  match mappings with
  | [] ->
    (* Shouldn't happen because this function is called on elements selected using the [link_selectors] list *)
    internal_error @@ Printf.sprintf
      "a relative_links or an absolute_links widget tried to process unsupported element <%s>" tag_name
  | _ ->
     let aux = fun (attr_name, attr_processor_wrapper) -> attr_processor_wrapper attr_processor attr_name elem in
     List.iter aux mappings

(** Checks a target against regexes it should and should not match to qualify for rewriting.

    The assumption is that [exclude_regex] is always defined: if the user doesn't supply one,
    [default_exclude_regex] is used.

    [only_regex] may or may not be defined, but if it's defined, then only links that match it
    are subject to rewriting. If users specify an overly broad [only_regex], it's their responsibility.
  *)
let target_matches only_regex exclude_regex target =
  match only_regex with
  | Some r ->
    if not (Regex_utils.Internal.matches ~regex:r target)
    then (let () = Logs.debug @@ fun m -> m "Link target \"%s\" does not match only_target_regex" target in false)
    else true
  | None ->
    if (Regex_utils.Internal.matches ~regex:exclude_regex target)
    then (let () = Logs.debug @@ fun m -> m "Link target \"%s\" matches exclude_target_regex" target in false)
    else true

(** Adjusts relative link targets to match the real depth of the page where they appear.

   For example, suppose templates/main.html has <img src="images/logo.png"> in it.
   The effective URL of the file is "example.com/images/logo.png".

   Now suppose a page at site/articles/goto.html is generated from that template.
   The "images/logo.png" path becomes invalid because it's not at the root level:
   it needs to be "../images/logo.png".

   This function rewrites paths according to the real depth by adding a "../" for every nesting level
   to make relative paths correct.
 *)
let relativize_link_target check_file env only_regex exclude_regex target =
  let open Defaults in
  if not (target_matches only_regex exclude_regex target) then
  let () =
    Logs.debug @@ fun m -> m "Link target \"%s\" is excluded by regex options, not trying to make it relative" target
  in target
  (* Before doing any real work, check if the link target is pointing at a file that actually exists
     at a path relative to _this_ page. If it does, the target is _already correct_
     and doesn't need to be relativized.

     If the target points at a path that doesn't exist, it _probably_ comes from a page template
     at the top level, while the current page is deeper in the directory tree.

     We are checking in the target rather than source dir for two reasons:
       1. soupault copies static assets from the site_dir before processing page files,
          so if an asset file exists in the site_dir, it's guaranteed to also be in the target_dir
          when this code runs;
       2. If a file is not in the site_dir, it doesn't mean it's not in the target_dir either.
          It may be a dynamically generated asset created by a Lua plugin or an external script.
   *)
  else if check_file && (Sys.file_exists (FilePath.concat env.target_dir target)) then target else
  (* Remove the build_dir from the target path to produce a path relative to the site root. *)
  let relative_target_dir = Regex_utils.Internal.replace ~regex:("^" ^ env.settings.build_dir) ~sub:"" env.target_dir in
  (* The assumption is that the target is valid for a page at the site root.
     Thus, for pages in sub-directories, we need to add a "../" for every nesting level.
   *)
  let parent_path = Utils.split_path relative_target_dir |> List.map (fun _ -> "..") in
  (* Strip leading slashes *)
  let target = Regex_utils.Internal.replace ~regex:"^/+" ~sub:"" target in
  (* Prepend generated double-dot path to the original target. *)
  String.concat "/" (parent_path @ [target])

(** Prepends a prefix (typically the base URL of the website) to link targets. *)
let absolutize_link_target prefix check_file env only_regex exclude_regex target =
  let open Defaults in
  if not (target_matches only_regex exclude_regex target) then
  let () =
    Logs.debug @@ fun m -> m "Link target \"%s\" is excluded by regex options, not trying to make it absolute" target
  in target
  (* Remove the build_dir from the path *)
  else let relative_target_dir = Regex_utils.Internal.replace ~regex:("^" ^ env.settings.build_dir) ~sub:"" env.target_dir in
  (* Strip leading slashes *)
  let target = Regex_utils.Internal.replace ~regex:"^/+" ~sub:"" target in
  let parent_path =
    (* If the target file path doesn't exist relative to the current level,
       the link probably comes from a page template and we just prepend the prefix to it,
       making an assumption that it's valid relative to the site root.
     *)
    (if check_file && (Sys.file_exists (FilePath.concat env.target_dir target))
     then let dir_path = Utils.split_path relative_target_dir in String.concat "/" (prefix :: dir_path)
     else prefix)
   in
   String.concat "/" [parent_path; target]

(** Converts all internal links to relative according to the page's location in the directory tree. *)
let relative_links env config soup =
  let valid_options = List.append Config.common_widget_options ["exclude_target_regex"; "only_target_regex"; "check_file"] in
  let () = Config.check_options valid_options config "widget \"relative_links\"" in
  let exclude_regex = OH.find_string_opt config ["exclude_target_regex"] in
  let only_regex = OH.find_string_opt config ["only_target_regex"] in
  if (Option.is_some exclude_regex) && (Option.is_some only_regex)
  then Config.config_error "exclude_target_regex and only_target_regex options are mutually exclusive"
  else begin
    let exclude_regex = Option.value ~default:default_exclude_regex exclude_regex in
    let check_file = Config.find_bool_or ~default:false config ["check_file"] in
    let nodes = Html_utils.select_all link_selectors soup in begin
    match nodes with
    | [] ->
      Logs.debug @@ fun m -> m "Page has no link elements that need adjustment"
    | ns ->
      let relativize_target = relativize_link_target check_file env only_regex exclude_regex in
      List.iter (fun elem -> process_attrs relativize_target elem) ns
    end;
    Ok ()
  end

(** Converts all internal links to absolute. *)
let absolute_links env config soup =
  let (let*) = Result.bind in
  let valid_options = List.append Config.common_widget_options
    ["exclude_target_regex"; "only_target_regex"; "check_file"; "prefix"]
  in
  let () = Config.check_options valid_options config "widget \"absolute_links\"" in
  let* prefix = Config.find_string_result config ["prefix"] in
  (* Strip trailing slashes to avoid duplicate slashes after concatenation *)
  let prefix = Regex_utils.Internal.replace ~regex:"/+$" ~sub:"" prefix in
  let exclude_regex = OH.find_string_opt config ["exclude_target_regex"] in
  let only_regex = OH.find_string_opt config ["only_target_regex"] in
  if (Option.is_some exclude_regex) && (Option.is_some only_regex)
  then Config.config_error "exclude_target_regex and only_target_regex options are mutually exclusive"
  else begin
    let exclude_regex = Option.value ~default:default_exclude_regex exclude_regex in
    let check_file = Config.find_bool_or ~default:false config ["check_file"] in
    let nodes = Html_utils.select_all link_selectors soup in begin
    match nodes with
    | [] ->
      Logs.debug @@ fun m -> m "Page has no link elements that need adjustment"
    | ns ->
      let absolutize_target = absolutize_link_target prefix check_file env only_regex exclude_regex in
      List.iter (fun elem -> process_attrs absolutize_target elem) ns
    end;
    Ok ()
  end

