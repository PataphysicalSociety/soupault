(* Link target manipulation widgets *)

open Soupault_common

let re_matches s pat =
  try
    let re = Re.Perl.compile_pat pat in
    let ms = Re.matches re s in
    List.length ms != 0
  with Re__Perl.Parse_error | Re__Perl.Not_supported ->
    soupault_error @@ Printf.sprintf "Malformed regex \"%s\"" pat

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

let relativize elem env check_file only_regex exclude_regex =
  let open Defaults in
  let target_matches target =
    (match only_regex with
     | Some r -> re_matches target r
     | None ->
       not (re_matches target exclude_regex))
  in
  let target_attr = get_target_attr elem in
  let target = Soup.attribute target_attr elem in
  match target with
  | None ->
    Logs.debug @@ fun m -> m "Ignoring a <%s> element without \"%s\" attribute" (Soup.name elem) target_attr
  | Some target ->
    if not (target_matches target)
    then Logs.debug @@ fun m -> m "Link target \"%s\" matches the exlude_regex, ignoring" target
    else begin
      (* Remove the build_dir from the path *)
      if check_file && (Sys.file_exists (FilePath.concat env.target_dir target)) then () else
      let relative_target_dir = Utils.regex_replace env.target_dir ("^" ^ env.settings.build_dir) "" in
      let dir_path = Utils.split_path relative_target_dir in
      let depth = List.length dir_path in
      let parent_path = make_parent_path depth in
      (* Strip leading slashes *)
      let target = Utils.regex_replace target "^/+" "" in
      let target = FilePath.concat parent_path target in
      Soup.set_attribute (get_target_attr elem) target elem
    end

(** Converts all internal links to relative according to the page's location in the directory tree. *)
let relative_links env config soup =
  let valid_options = List.append Config.common_widget_options ["exclude_target_regex"; "only_target_regex"; "check_file"] in
  let () = Config.check_options valid_options config "widget \"relative_links\"" in
  let exclude_regex = Config.get_string_default "exclude_target_regex" "^((([a-zA-Z0-9]+):)|#)" config in
  let only_regex = Config.get_string_opt "only_target_regex" config in
  let check_file = Config.get_bool_default false "check_file" config in
  let nodes = Html_utils.select_all link_selectors soup in
  begin
    match nodes with
    | [] ->
       Logs.debug @@ fun m -> m "Page has no link elements that need adjustment"
    | ns -> List.iter (fun e -> relativize e env check_file only_regex exclude_regex) ns
  end;
  Ok ()

