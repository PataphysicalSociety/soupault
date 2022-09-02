open Defaults
open Soupault_common

let page_matches site_dir actual_path conf_path =
  let conf_path = FilePath.concat site_dir conf_path in
  (=) conf_path actual_path

let regex_matches actual_path path_re =
  try Regex_utils.Raw.matches ~regex:path_re actual_path
  with Regex_utils.Bad_regex ->
    soupault_error @@ Printf.sprintf "Could not check a path regex option: malformed regex \"%s\"" path_re

(* Normally a directory is a section.
   However, some directories can be in fact hand-made "clean URLs".
   One valid reason to do that is to keep page-specific assets together with pages.

   We provide a way to mark a directory as "leaf rather than branch"
   using a configurable "leaf file".
 *)
let has_leaf_file settings page_file =
  match settings.index_leaf_file with
  | None -> false
  | Some lf ->
    FileUtil.test FileUtil.Exists @@ FilePath.concat (FilePath.dirname page_file) lf

let is_handmade_clean_url settings page_file =
  let page_name = FilePath.basename page_file |> FilePath.chop_extension in
  (* If it doesn't look like an index page, then there's no question: it's a normal page. *)
  if (page_name <> settings.index_page) then false else
  (* If a page is in the forced_indexing_path_regex, we treat it as a normal page regardless of its file name. *)
  if (List.exists (regex_matches page_file) settings.index_force) then
    let () = Logs.debug @@ fun m -> m "Forced indexing is enabled for page %s" page_file in
    true
  (* Another way to force an "index" page to be treated as a normal page
     is to create a "leaf market" file in its directory,
     if enabled by the index.leaf_file option. *)
  else if (has_leaf_file settings page_file) then
    let () = Logs.debug @@ fun m -> m "Directory with page %s contains a leaf marker file, treating as a non-index page" page_file in
    true
  else false

let section_matches ?(include_subsections=false) settings site_dir actual_path conf_path =
   (* Remove trailing slashes *)
   let conf_path = FilePath.concat site_dir conf_path |> File_path.normalize_path in
   let page_dir = FilePath.dirname actual_path in
   let page_dir =
     if (is_handmade_clean_url settings actual_path)
     then FilePath.dirname page_dir
     else page_dir
   in
   (* is_subdir doesn't consider a dir its own subdir, so we need to handle the same dir case explicitly. *)
   (include_subsections && (FilePath.is_subdir page_dir conf_path))  || (conf_path = page_dir)

let page_included settings options site_dir page_file =
  if (List.exists (regex_matches page_file) options.regexes_exclude) ||
     (List.exists (page_matches site_dir page_file) options.pages_exclude)  ||
     (List.exists (section_matches ~include_subsections:options.include_subsections settings site_dir page_file) options.sections_exclude)
  then false
  else match options.pages, options.sections, options.regexes with
  | [], [], [] -> true
  | _, _, _ ->
    (List.exists (regex_matches page_file) options.regexes) ||
    (List.exists (page_matches site_dir page_file) options.pages) ||
    (List.exists (section_matches ~include_subsections:options.include_subsections settings site_dir page_file) options.sections)

let is_default opts =
  (* The include_subsections option only makes sense when section/exclude_section is also set.
     A config with just include_subsection but no other options is considered default,
     for the decision whether to use page targeting options or not.
   *)
  let opts = {opts with include_subsections=default_path_options.include_subsections} in
  opts = default_path_options
