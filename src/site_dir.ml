open Defaults

module FP = FilePath
module FU = FileUtil

let list_dirs path =
    FU.ls path |> FU.filter FU.Is_dir

let remove_ignored_files settings files =
  let ignored settings file =
    (Utils.any_in_list settings.ignore_extensions (File_path.get_extensions file)) ||
    (List.exists (fun r -> Regex_utils.Public.matches ~regex:r file) settings.ignore_path_regexes)
  in
  List.filter (fun f -> not (ignored settings f)) files

let list_section_files settings path =
  let is_page_file f = Utils.in_list (File_path.get_extension f) settings.page_extensions in
  let files = FU.ls path |> FU.filter (FU.Is_file) |> remove_ignored_files settings in
  let page_files = List.find_all is_page_file files in
  let other_files = List.find_all (fun f -> not (is_page_file f)) files in
  page_files, other_files

(* Build a list of site source files to be processed.

   There are two kinds of files: page files and asset files.
   Additionally we split page files into section index files
   and normal pages.

   Page files are parsed into HTML element trees
   and manipulated in various ways.

   Asset files are simply copied over to their new location.
 *)
let get_site_files settings =
  let rec aux path nav_path =
    let dirs = FU.ls path |> FU.filter FU.Is_dir in
    (* Remove ignored dirs *)
    let dirs = List.filter (fun d -> not (Utils.in_list (FP.basename d) settings.ignore_directories)) dirs in
    let section_page_files, section_asset_files = list_section_files settings path in
    let asset_path = File_path.concat_path (settings.build_dir :: nav_path) in
    let section_asset_files = List.map (fun x -> (x, asset_path)) section_asset_files in
    (* Collect files from subdirs *)
    List.fold_left
      (fun (p, a) d ->
        let (p', a') = aux d (nav_path @ [FP.basename d]) in
        (p @ p'), (a @ a'))
      (section_page_files, section_asset_files)
      dirs
  in aux settings.site_dir []

