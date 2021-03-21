(* Link target manipulation widgets *)

open Soupault_common

(* By default, exclude three categories of links from target rewriting:
     1. Links that have a URI schema (^([a-zA-Z0-9]+):), e.g. https://example.com
     2. Links to anchors within the same page (^#), e.g. #my-anchor
     3. Hand-made relative links (^\.), e.g. ../style.css
 *)
let default_exclude_regex = "^((([a-zA-Z0-9]+):)|#|\\.)"

let link_selectors = ["a"; "link"; "img"; "script"; "audio"; "video"; "object"; "embed"]

let get_target_attr elem =
  let tag_name = Soup.name elem in
  match tag_name with
  | "a" -> "href"
  | "link" -> "href"
  | "img" -> "src"
  | "script" -> "src"
  | "audio" -> "src"
  | "video" -> "src"
  | "embed" -> "src"
  | "object" -> "data"
  | _ ->
    (* Can't happen *)
    soupault_error @@ Printf.sprintf
      "Internal error: relative_links widget tried to process an unsupported element <%s>" tag_name

let make_parent_path depth = Containers.String.repeat "../" depth

let re_matches s pat =
  try
    let re = Re.Perl.compile_pat pat in
    let ms = Re.matches re s in
    List.length ms != 0
  with Re__Perl.Parse_error | Re__Perl.Not_supported ->
    soupault_error @@ Printf.sprintf "Malformed regex \"%s\"" pat

let target_matches only_regex exclude_regex target =
  match only_regex with
  | Some r -> re_matches target r
  | None ->
    not (re_matches target exclude_regex)

let relativize elem env check_file only_regex exclude_regex =
  let open Defaults in
  let target_attr = get_target_attr elem in
  let target = Soup.attribute target_attr elem in
  match target with
  | None ->
    Logs.debug @@ fun m -> m "Ignoring a <%s> element without \"%s\" attribute" (Soup.name elem) target_attr
  | Some target ->
    if not (target_matches only_regex exclude_regex target)
    then Logs.debug @@ fun m -> m "Link target \"%s\" matches the exlude_target_regex, ignoring" target
    else begin
      (* Remove the build_dir from the path *)
      if check_file && (Sys.file_exists (FilePath.concat env.target_dir target)) then () else
      let relative_target_dir = Utils.regex_replace env.target_dir ("^" ^ env.settings.build_dir) "" in
      let dir_path = Utils.split_path relative_target_dir in
      let depth = List.length dir_path in
      let parent_path = make_parent_path depth in
      (* Strip leading slashes *)
      let target = Utils.regex_replace target "^/+" "" in
      let target = String.concat "/" [parent_path; target] in
      Soup.set_attribute (get_target_attr elem) target elem
    end

let absolutize elem env prefix check_file only_regex exclude_regex =
  let open Defaults in
  let target_attr = get_target_attr elem in
  let target = Soup.attribute target_attr elem in
  match target with
  | None ->
    Logs.debug @@ fun m -> m "Ignoring a <%s> element without \"%s\" attribute" (Soup.name elem) target_attr
  | Some target ->
    if not (target_matches only_regex exclude_regex target)
    then Logs.debug @@ fun m -> m "Link target \"%s\" matches the exlude_target_regex, ignoring" target
    else begin
      (* Remove the build_dir from the path *)
      let relative_target_dir = Utils.regex_replace env.target_dir ("^" ^ env.settings.build_dir) "" in
      (* Strip leading slashes *)
      let target = Utils.regex_replace target "^/+" "" in
      let parent_path =
        (if check_file && (Sys.file_exists (FilePath.concat env.target_dir target))
        then let dir_path = Utils.split_path relative_target_dir in String.concat "/" (prefix :: dir_path)
        else prefix)
      in
      let target = String.concat "/" [parent_path; target] in
      Soup.set_attribute (get_target_attr elem) target elem
    end

(** Converts all internal links to relative according to the page's location in the directory tree. *)
let relative_links env config soup =
  let valid_options = List.append Config.common_widget_options ["exclude_target_regex"; "only_target_regex"; "check_file"] in
  let () = Config.check_options valid_options config "widget \"relative_links\"" in
  let exclude_regex = Config.get_string_opt "exclude_target_regex" config in
  let only_regex = Config.get_string_opt "only_target_regex" config in
  if (Option.is_some exclude_regex) && (Option.is_some only_regex)
  then Config.config_error "exclude_target_regex and only_target_regex options are mutually exclusive"
  else begin
    let exclude_regex = Option.value ~default:default_exclude_regex exclude_regex in
    let check_file = Config.get_bool_default false "check_file" config in
    let nodes = Html_utils.select_all link_selectors soup in begin
    match nodes with
    | [] ->
      Logs.debug @@ fun m -> m "Page has no link elements that need adjustment"
    | ns -> List.iter (fun e -> relativize e env check_file only_regex exclude_regex) ns
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
  let* prefix = Config.get_string_result "Missing required_option \"prefix\"" "prefix" config in
  (* Strip trailing slashes to avoid duplicate slashes after concatenation *)
  let prefix = Utils.regex_replace prefix "/+$" "" in
  let exclude_regex = Config.get_string_opt "exclude_target_regex" config in
  let only_regex = Config.get_string_opt "only_target_regex" config in
  if (Option.is_some exclude_regex) && (Option.is_some only_regex)
  then Config.config_error "exclude_target_regex and only_target_regex options are mutually exclusive"
  else begin
    let exclude_regex = Option.value ~default:default_exclude_regex exclude_regex in
    let check_file = Config.get_bool_default false "check_file" config in
    let nodes = Html_utils.select_all link_selectors soup in begin
    match nodes with
    | [] ->
      Logs.debug @@ fun m -> m "Page has no link elements that need adjustment"
    | ns -> List.iter (fun e -> absolutize e env prefix check_file only_regex exclude_regex) ns
    end;
    Ok ()
  end

