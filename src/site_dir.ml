open Defaults

module FP = FilePath
module FU = FileUtil

(*** Filesystem stuff ***)
let (+/) left right =
    FP.concat left right

let list_dirs path =
    FU.ls path |> FU.filter FU.Is_dir

let remove_ignored_files settings files =
  let ignored settings file = Utils.in_list settings.ignore_extensions (Utils.get_extension file) in
  List.filter (fun f -> not (ignored settings f)) files

let list_section_files settings path =
  let is_page_file f = Utils.in_list settings.page_extensions (Utils.get_extension f) in
  let files = FU.ls path |> FU.filter (FU.Is_file) |> remove_ignored_files settings in
  let page_files = List.find_all is_page_file files in
  let other_files = List.find_all (fun f -> not (is_page_file f)) files in
  page_files, other_files

let split_pages settings ps =
  let find_index = fun p -> (FP.basename p |> FP.chop_extension) = settings.index_page in
  let index_page = CCList.find_opt find_index ps in
  match index_page with
  | None -> ps, []
  | Some p ->
    if Autoindex.index_extraction_should_run settings p then
      (* The main idea of this option is to allow hand-made clean URLs.
         If it's an index page, but force_indexing_path_regex or a leaf file market
         forced treating it as a normal page, we ignore the fact that it looks like
         an index page.
       *)
      let () = Logs.debug @@ fun m -> m "Index page %s is treated as a normal page" p in
      ps, []
    else
      let ps = CCList.remove ~eq:(=) ~key:p ps in
      ps, [p]

(* Build a list of site source files to be processed.

   There are two kinds of files: page files and asset files.
   Additionally we split page files into section index files
   and normal pages.

   Asset files are simply copied over to their new location.

   Page files are converted to HTML (if they aren't HTML already),
   parsed and modified. We also extract metadata from them.

   Metadata extracted from normal pages is inserted into section
   index pages.
   This means we can only process index pages after all normal pages
   are processed, and this is why we build a separate list of index pages.
 *)
let get_site_files settings =
  let rec aux path nav_path =
    let dirs = FU.ls path |> FU.filter FU.Is_dir in
    let section_page_files, section_asset_files = list_section_files settings path in
    let section_page_files, section_index_files = split_pages settings section_page_files in
    (* Attach the nav path to each file. Target dir and page URL are generated from it *)
    let section_page_files = List.map (fun x -> (x, nav_path)) section_page_files in
    let	section_index_files = List.map (fun x -> (x, nav_path)) section_index_files in
    let asset_path = Utils.concat_path (settings.build_dir :: nav_path) in
    let section_asset_files = List.map (fun x -> (x, asset_path)) section_asset_files in
    (* Collect files from subdirs *)
    List.fold_left
      (fun (p, i, a) d ->
        let (p', i', a') = aux d (nav_path @ [FP.basename d]) in
        (p @ p'), (i @ i'), (a @ a'))
      (section_page_files, section_index_files, section_asset_files)
      dirs
  in aux settings.site_dir []
