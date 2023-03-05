(* Soupault allows the user to restrict widgets and hooks to a subset of all pages:
   the user can tell them to run only on certain pages, exclude certain pages,
   or mix those criteria.

   - page/exclude page — for matching exact pages.
   - section/exclude_section — for matching all pages within a directory
   - path_regex/exclude_path_regex — for matching arbitrary sets of pages using a regular expression

   Those options can be combined, and if they all are present in the same widget/hook,
   then the page is checked against every option,
   i.e., the page is excluded if it matches any exclude_* option
   and considered included if it matches any inclusion option
   (in other words, there is no inclusion option precedence).

   One more thing to note is that the [section] option only applies to files in a directory
   but not to its subdirectories.
   To make a widget apply to a directory tree, the user needs to add [include_subsections=true].
  *)

open Defaults
open Soupault_common

(* The underlying function of the [page/exclude_page] option.
   [page = "foo/bar.html"] enabled widget/hook iff
   the page source path is "foo/bar.html".
 *)
let page_matches site_dir actual_path conf_path =
  let conf_path = FilePath.concat site_dir conf_path in
  (=) conf_path actual_path

(* The underlying function of the [pathregex/exclude_path_regex] option. *)
let regex_matches actual_path path_re =
  try Regex_utils.Raw.matches ~regex:path_re actual_path
  with Regex_utils.Bad_regex ->
    soupault_error @@ Printf.sprintf {|Could not check a path regex option: malformed regex "%s"|} path_re

(* The underlying function of the [section/exclude_section] option.

   Section check is more complicated because it needs to tell apart directories that are actually sections
   and hand-made clean URLs.
   (Hugo calls them "branch bundles" and "leaf bundles").

   Normally, a directory is a section.
   However, web servers have no distinction and the user may want to create foo/index.html instead of site/foo.html.
   One valid reason to do that is to keep page-specific assets together with pages.

   Soupault provides two ways to mark a directory as a leaf rather than a branch.
   The first way is to use a "leaf file".
   If that file is present, the directory is treated as a hand-made clean URL.
   By default the leaf file is ".leaf" but it's configurable in [index.leaf_file].

   The second way is to add a regex for pages that must be treated as content pages rather than section index pages
   to [index.force_indexing_path_regex].
   Pages that match that regex are treated as content pages regardless of their names.
 *)
let is_handmade_clean_url settings page_file =
  (* Checks if a leaf file exists. *)
  let has_leaf_file settings page_file =
    match settings.index_leaf_file with
    | None -> false
    | Some lf ->
      FileUtil.test FileUtil.Exists @@ FilePath.concat (FilePath.dirname page_file) lf
  in
  let page_name = FilePath.basename page_file |> FilePath.chop_extension in
  (* If it doesn't look like an index page, then there's no question: it's a normal (content) page. *)
  if (page_name <> settings.index_page) then false else
  (* If a page matches index.forced_indexing_path_regex, we treat it as a normal page regardless of its file name. *)
  if (List.exists (regex_matches page_file) settings.index_force) then
    let () = Logs.debug @@ fun m -> m "Forced indexing is enabled for page %s" page_file in
    true
  (* And if it contains a leaf file, then we treat it as a content page as well. *)
  else if (has_leaf_file settings page_file) then
    let () = Logs.debug @@ fun m -> m "Directory with page %s contains a leaf marker file, treating it as a non-index page" page_file in
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
   (* FilePath.is_subdir doesn't consider a dir its own subdir, so we need to handle that case explicitly. *)
   (include_subsections && (FilePath.is_subdir page_dir conf_path)) || (conf_path = page_dir)

(* The high-level page check function that handles all path options. *)
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

(* The default behavior for an index page foo/index.html is to include
   everything under [foo/*.*] and [foo/*/*.*] in its view
   (i.e. "current section and all its subsections").

   This function checks if there are any significant options set for a view
   or its config is effectively default.
 *)
let is_default opts =
  (* [include_subsections=true] option only makes a difference when [section/exclude_section] is also set —
     otherwise it's implied.
     A config with just [include_subsections] but no other options is thus considered default,
     for the purpose of deciding whether to use page targeting options or not.
   *)
  let opts = {opts with include_subsections=default_path_options.include_subsections} in
  opts = default_path_options
