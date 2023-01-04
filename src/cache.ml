open Soupault_common
open Defaults

let hash_sum s =
  let ctx = Digestif.BLAKE2S.empty in
  let ctx = Digestif.BLAKE2S.feed_string ctx s in
  Digestif.BLAKE2S.get ctx |> Digestif.BLAKE2S.to_hex

let read_file path =
  try Soup.read_file path
  with Sys_error msg ->
    Printf.ksprintf soupault_error "Could not read a cached object: %s" msg

let write_file path data =
  try Soup.write_file path data
  with Sys_error msg ->
    Printf.ksprintf soupault_error "Could not save a cached object: %s" msg

let make_cached_object_path settings page_path str =
  let hash = hash_sum str in
  let page_cache_path = FilePath.concat page_path hash in
  let object_path = FilePath.concat settings.cache_dir page_cache_path in
  object_path

let get_cached_object settings page_path str =
  if (not settings.caching) || (settings.force) then None else
  let object_path = make_cached_object_path settings page_path str in
  if FileUtil.test FileUtil.Exists object_path
  then
    let () = Logs.debug @@ fun m -> m "Reading a cached object from %s" object_path in
    Some (read_file object_path) 
  else None

let cache_object settings page_path source_str output_str =
  if not settings.caching then () else
  (* If caching is on, we save the cached object to a file at
     [<cache dir>/<path path>/<hash>],
     like [.cache/about.md/69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9]
   *)
  let target_path = make_cached_object_path settings page_path source_str in
  Logs.debug @@ fun m -> m "Saving a cached object to %s" target_path;
  write_file target_path output_str

let is_cache_outdated settings page_path page_source =
  if settings.force then true else
  try
    let page_hash_path = FilePath.concat (FilePath.concat settings.cache_dir page_path) Defaults.page_hash_file in
    (* If the page source hash file is missing, assume that the cache is invalid. *)
    if not (FileUtil.test FileUtil.Exists page_hash_path) then
      let () = Logs.warn @@ fun m -> m "Cache directory for page %s does not contain a page source hash file (%s),\
        cache will be discarded!" page_path page_hash_file
      in
      true
    else
      let cached_page_hash = read_file page_hash_path |> String.trim in
      let current_page_hash = hash_sum page_source in
      cached_page_hash <> current_page_hash
  with Sys_error msg ->
    Printf.ksprintf soupault_error "Could not check the hash sum file for %s: %s" page_path msg

let refresh_page_cache settings page_path page_source =
  let save_page_hash page_cache_path page_source =
    let page_hash = hash_sum page_source in
    let target_file = FilePath.concat page_cache_path Defaults.page_hash_file in
    Logs.debug @@ fun m -> m "Saving new page hash to %s" target_file;
    write_file target_file page_hash
  in
  if not settings.caching then () else
  let page_cache_path = FilePath.concat settings.cache_dir page_path in
  if FileUtil.test FileUtil.Exists page_cache_path then
    begin
      if is_cache_outdated settings page_path page_source then
        begin
	  Logs.debug @@ fun m -> m "Cleaning outdated cache for page %s" page_path;
          FileUtil.rm ~force:FileUtil.Force ~recurse:true [page_cache_path];
          FileUtil.mkdir ~parent:true page_cache_path;
          save_page_hash page_cache_path page_source
        end
      else
	Logs.debug @@ fun m -> m "Cache for page %s is considered valid and will be used" page_path
    end
  else
    begin
      FileUtil.mkdir ~parent:true page_cache_path;
      save_page_hash page_cache_path page_source
    end
