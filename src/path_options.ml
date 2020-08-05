open Defaults

let page_matches site_dir actual_path conf_path =
  let conf_path = FilePath.concat site_dir conf_path in
  (=) conf_path actual_path

let section_matches ?(include_subsections=false) site_dir actual_path conf_path =
   (* Remove trailing slashes *)
   let conf_path = FilePath.concat site_dir conf_path |> Re.replace (Re.Perl.compile_pat "/+$") ~f:(fun _ -> "") in
   let page_dir = FilePath.dirname actual_path in
   (* is_subdir doesn't consider a dir its own subdir,
      so we need to handle the same dir case explicitly.

      Moreover, it returns a false positive if the child matches the beginning but doesn't have a trailing slash,
      so here's this fixup.
    *)
   (include_subsections && (FilePath.is_subdir conf_path page_dir)) || (conf_path = page_dir)

let regex_matches actual_path path_re =
  let matches = Utils.get_matching_strings path_re actual_path in
  match matches with
  | Ok ms -> List.length ms <> 0
  | Error msg ->
    let () =
      Logs.warn @@ fun m -> m "Failed to check page %s against regex \"%s\" (malformed regex?), assuming false: %s"
      actual_path path_re msg
    in
    false

let page_included options site_dir page_file =
  if (List.exists (regex_matches page_file) options.regexes_exclude) ||
     (List.exists (page_matches site_dir page_file) options.pages_exclude)  ||
     (List.exists (section_matches ~include_subsections:options.include_subsections site_dir page_file) options.sections_exclude)
  then false
  else match options.pages, options.sections, options.regexes with
  | [], [], [] -> true
  | _, _, _ ->
    let should_run =
      (List.exists (regex_matches page_file) options.regexes) ||
      (List.exists (page_matches site_dir page_file) options.pages) ||
      (List.exists (section_matches ~include_subsections:options.include_subsections site_dir page_file) options.sections)
    in
    let () =
      if not should_run then
      Logs.debug @@ fun m -> m "Page %s does not match any page/section/regex options" page_file
    in should_run

let is_default opts =
  (* The include_subsections option only makes sense when section/exclude_section is also set.
     A config with just include_subsection but no other options is considered default,
     for the decision whether to use page targeting options or not.
   *)
  let opts = {opts with include_subsections=default_path_options.include_subsections} in
  opts = default_path_options
