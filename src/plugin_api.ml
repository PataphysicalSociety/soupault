open Soupault_common

exception Plugin_error of string
exception Plugin_exit of string option

let plugin_error err = raise (Plugin_error err)

(* Please note that these functions use "string, regex" argument order.
   This inconsistency with internal regex functions is unfortunate,
   but it's been this way since the introduction of the Lua API
   and will have to stay this way.
 *)
module Re_wrapper = struct
  let replace ?(all=false) s pat sub =
    try
      Regex_utils.Raw.replace ~all:all ~regex:pat ~sub:sub s
    with Regex_utils.Bad_regex ->
      plugin_error @@ Printf.sprintf {|Malformed regex "%s" in a Regex.replace call|} pat

  let replace_all s pat sub =
    try
      Regex_utils.Raw.replace ~all:true ~regex:pat ~sub:sub s
    with Regex_utils.Bad_regex ->
      plugin_error @@ Printf.sprintf {|Malformed regex "%s" in a Regex.replace_all call|} pat

  let find_all s pat =
    try
      Regex_utils.Raw.get_matching_strings ~regex:pat s
    with Regex_utils.Bad_regex ->
      plugin_error @@ Printf.sprintf {|Malformed regex "%s" in a Regex.find_all call|} pat

  let re_match s pat =
    try
      Regex_utils.Raw.matches ~regex:pat s
    with Regex_utils.Bad_regex ->
      plugin_error @@ Printf.sprintf {|Malformed regex "%s" in a Regex.match call|} pat

  let split s pat =
    try
      Regex_utils.Raw.split ~regex:pat s
    with Regex_utils.Bad_regex ->
      plugin_error @@ Printf.sprintf {|Malformed regex "%s" in a Regex.split call|} pat
end

module Log = struct
  let debug s = Logs.debug @@ fun m -> m "%s" s
  let info s = Logs.info @@ fun m -> m "%s" s
  let warning s = Logs.warn @@ fun m -> m "%s" s
  let error s = Logs.err @@ fun m -> m "%s" s
end

module Sys_wrappers = struct
  let read_file name =
    try Some (Soup.read_file name)
    with
    | Sys_error msg -> let () = Logs.err @@ fun m -> m {|Failed to read file "%s": %s|} name msg in None

  let write_file name content =
    try Soup.write_file name content
    with Sys_error msg ->
      Printf.ksprintf plugin_error {|Failed to write file "%s": %s|} name msg

  let get_program_output cmd input =
    let res = Process_utils.get_program_output ~input:input cmd in
    match res with
    | Ok o -> Some o
    | Error msg ->
      let () = Logs.err @@ fun m -> m "%s" msg in  
      None

  let run_program cmd =
    let res = Process_utils.get_program_output cmd in
    match res with
    | Ok _ -> Some 1
    | Error msg ->
      let () = Logs.err @@ fun m -> m "%s" msg in
      None

  let run_program_get_exit_code cmd =
    let res = Process_utils.get_program_output_raw cmd in
    match res with
    | Ok _ -> 0
    | Error status ->
      let () = Logs.err @@ fun m -> m "%s" (Process_utils.format_error cmd status) in
      Process_utils.exit_code_of_status status

  let getenv name default =
    let res = Sys.getenv_opt name in
    match res with
    | Some _ -> res
    | None ->
      begin
        match default with
        | Some _ -> default 
        | None -> None
      end

  let delete_file ?(r=false) path =
    try FileUtil.rm ~recurse:r [path]
    with Unix.Unix_error (e, _, _) ->
      Printf.ksprintf plugin_error {|Could not delete file "%s": %s|} path (Unix.error_message e)

  let ls path =
    try FileUtil.ls path
    with Sys_error msg ->
      Printf.ksprintf plugin_error {|Failed to get directory contents if "%s": %s|} path msg

  let get_extension f =
    try File_path.get_extension f
    with Malformed_file_name name ->
      Printf.ksprintf plugin_error {|Malformed file name "%s" in a call to Sys.get_extension|} name

  let get_extensions f =
    try	File_path.get_extensions f
    with Malformed_file_name name ->
      Printf.ksprintf plugin_error {|Malformed file name "%s" in a call to Sys.get_extensions|} name

  let has_extension e f =
    try	File_path.has_extension e f
    with Malformed_file_name name	->
      Printf.ksprintf plugin_error {|Malformed file name "%s" in a call to Sys.has_extension|} name

  let strip_extensions f =
    try File_path.strip_extensions f
    with Malformed_file_name name ->
      Printf.ksprintf plugin_error {|Malformed file name "%s" in a call to Sys.strip_extensions|} name
end

module Plugin_version = struct
  let require_version vstr =
    try
     let res = Version.require_version vstr in
     if res then () else
     let msg = Printf.sprintf "Plugin requires soupault %s or newer, current version is %s" vstr Defaults.version_string in
     raise (Plugin_error msg)
    with Failure msg ->
      plugin_error @@ Printf.sprintf "Plugin.require_version failed: %s" msg
end

module Html = struct
  let (let*) = Stdlib.Option.bind
  let return = Stdlib.Option.some

  type soup_wrapper = 
    | GeneralNode of Soup.general Soup.node
    | ElementNode of Soup.element Soup.node
    | SoupNode of Soup.soup Soup.node

  type 'a t = soup_wrapper

  let from_soup s = SoupNode s

  let from_element e = ElementNode e

  let to_element n =
    match n with
    | ElementNode n -> n
    | GeneralNode n ->
      let n' = Soup.element n in begin
        match n' with
        | Some e -> e
        | None -> plugin_error "Expected an HTML element, but found a document or a text node"
      end
    | _ -> plugin_error "Expected an HTML element node, but found a document"

  let is_element n =
    match n with
    | ElementNode _ -> true
    | GeneralNode n -> Soup.is_element n
    | _ -> false

  let to_general n =
    match n with
    | GeneralNode n -> n
    | ElementNode n -> Soup.coerce n
    | SoupNode n -> Soup.coerce n

  let to_soup n =
    match n with
    | SoupNode n -> n
    | _ -> plugin_error "Expected an HTML document but found an element or a text node"

  let select soup selector =
    let* soup = soup in
    let* elems =
      try to_general soup |> Soup.select selector |> Soup.to_list |> List.map (fun x -> ElementNode x) |> return
      with Soup.Parse_error msg ->
        plugin_error @@ Printf.sprintf "HTML.select called with invalid CSS selector '%s': %s" selector msg
    in Some elems

  let select_one soup selector =
    let* soup = soup in
    let* n =
      try to_general soup |> Soup.select_one selector
      with Soup.Parse_error msg ->
        plugin_error @@ Printf.sprintf "HTML.select_one called with invalid CSS selector '%s': %s" selector msg
    in Some (ElementNode n)

  let select_any_of soup selectors =
    let* soup = soup in
    let* n =
      try to_general soup |> Html_utils.select_any_of selectors
      with Soupault_error msg -> plugin_error msg
    in Some (ElementNode n)

  let select_all_of soup selectors =
    let* soup = soup in
    try to_general soup |> Html_utils.select_all selectors |> List.map (fun x -> ElementNode x) |> return
    with Soupault_error msg ->
      plugin_error msg

  let matches_selector soup elem selector =
    try Soup.matches_selector (to_soup soup) selector (to_element elem)
    with Soupault_error msg ->
      plugin_error msg

  let matches_any_of_selectors soup elem selectors =
    try Html_utils.matches_any_of selectors (to_soup soup) (to_element elem)
    with Soupault_error msg ->
      plugin_error msg

  let children node =
    let* node = node in
    to_general node |> Soup.children |> Soup.to_list |> List.map (fun x -> GeneralNode x) |> return

  let descendants node =
    let* node = node in
    to_general node |> Soup.descendants |> Soup.to_list |> List.map (fun x -> GeneralNode x) |> return

  let ancestors node =
    let* node = node in
    to_general node |> Soup.ancestors |> Soup.to_list |> List.map (fun x -> ElementNode x) |> return

  let siblings node =
    let* node = node in
    to_general node |> Soup.siblings |> Soup.to_list |> List.map (fun x -> GeneralNode x) |> return

  let get_attribute node attr_name =
    let* node = node in
    to_element node |> Soup.attribute attr_name

  let set_attribute node attr_name attr_value =
    match node with
    | None -> ()
    | Some node -> to_element node |> Soup.set_attribute attr_name attr_value

  let append_attribute node attr_name attr_value =
    match node with
    | None -> ()
    | Some node ->
      let node = to_element node in
      let old_attr_value = Soup.attribute attr_name node |> Option.value ~default:"" in
      let attr_value = old_attr_value ^ attr_value in
      Soup.set_attribute attr_name attr_value node

  let delete_attribute node attr_name =
    match node with
    | None -> ()
    | Some node -> to_element node |> Soup.delete_attribute attr_name

  let list_attributes node =
    let aux e = Soup.fold_attributes (fun acc name _ -> name :: acc) [] e in
    match node with
    | None -> []
    | Some node -> to_element node |> aux

  let clear_attributes node =
    let aux e =
      Soup.fold_attributes (fun acc name _ -> name :: acc) [] e |>
      List.iter (fun n -> Soup.delete_attribute n e)
    in
    match node with
    | None -> ()
    | Some node -> to_element node |> aux

  let add_class node class_name =
    match node with
    | None -> ()
    | Some node -> to_element node |> Soup.add_class class_name

  let remove_class node class_name =
    match node with
    | None -> ()
    | Some node -> to_element node |> Soup.remove_class class_name

  let get_classes node =
    match node with
    | None -> plugin_error "HTML.get_classes was called on a nil value"
    | Some node -> to_element node |> Soup.classes

  let has_class node class_name =
    match node with
    | None -> plugin_error "HTML.has_class was called on a nil value"
    | Some node ->
      to_element node |> Soup.classes |> List.find_opt ((=) class_name) |> Option.is_some

  let do_with_node func node child =
    match node with
    | None -> ()
    | Some node -> func node child

  let parent node =
    let* node = node in
    let* n = to_element node |> Soup.parent in
    ElementNode n |> return

  let append_child node child =
    let child = to_general child in
    match node with
    | ElementNode n -> Soup.append_child n child
    | SoupNode n -> Soup.append_root n child
    | GeneralNode _ as n -> Soup.append_child (to_element n) child

  let prepend_child node child =
    let child = to_general child in
    match node with
    | ElementNode n -> Soup.prepend_child n child
    | SoupNode _ -> plugin_error "Cannot prepend a child to a document node"
    | GeneralNode _ as n -> Soup.prepend_child (to_element n) child

  let insert_before node child =
    let child = to_general child in
    match node with
    | ElementNode n -> Soup.insert_before n child
    | SoupNode _ -> plugin_error "Cannot use HTML.insert_before with a document node"
    | GeneralNode _ as n -> Soup.insert_before (to_element n) child

  let insert_after node child =
    let child = to_general child in
    match node with
    | ElementNode n -> Soup.insert_after n child
    | SoupNode _ -> plugin_error "Cannot use HTML.insert_after with a document node"
    | GeneralNode _ as n -> Soup.insert_after (to_element n) child

  let replace node child =
    let child = to_general child in
    match node with
    | ElementNode n -> Soup.replace n child
    | SoupNode _ -> plugin_error "Cannot use HTML.replace with a document node"
    | GeneralNode _ as n -> Soup.replace (to_element n) child

  let replace_content node child =
    let child = to_general child in
    match node with
    | ElementNode n -> Html_utils.replace_content n child
    | SoupNode _ -> plugin_error "Cannot use HTML.replace_content with a document node"
    | GeneralNode _ as n -> Html_utils.replace_content (to_element n) child

  let append_root node child =
    let node = to_soup node in
    let child = to_general child in
    Soup.append_root node child

  let prepend_root node child =
    let node = to_soup node in
    let child = to_general child in
    Soup.prepend_root node child

  let delete_content node =
    match node with
    | None -> ()
    | Some node -> to_general node |> Soup.clear

  let get_tag_name node =
    let* node = node in
    match node with
    | ElementNode n -> Soup.name n |> return
    | GeneralNode _ as n -> Soup.name (to_element n) |> return
    | _ -> plugin_error "Cannot get tag name: node is an HTML document"

  let set_tag_name node name =
    match node with
    |None -> ()
    | Some n ->
      begin
        match n with
        | ElementNode n -> Soup.set_name name n
        | SoupNode _ -> plugin_error "Document node does not have a tag name"
        | GeneralNode _ -> plugin_error "Cannot set tag name of a general node"
      end

  let delete node =
    match node with
    | None -> ()
    | Some node -> to_general node |> Soup.delete

  let create_element name text =
    let text = Option.value ~default:"" text in
    ElementNode (Soup.create_element ~inner_text:text name)

  let create_text text = GeneralNode (Soup.create_text text)

  let inner_html node =
    match node with
    | None -> ""
    | Some node -> to_general node |> Html_utils.inner_html

  let strip_tags node =
    match node with
    | None -> ""
    | Some node -> to_general node |> Html_utils.get_element_text |> Option.value ~default:""

  let clone_content node =
    let* node = node in
    SoupNode (to_general node |> Html_utils.child_nodes) |> return

  let clone_page node =
    let node = to_soup node in
    SoupNode (Soup.to_string node |> Soup.parse)

  let unwrap node =
    match node with
    | None -> ()
    | Some node -> to_element node |> Soup.unwrap

  let child_count node =
    match node with
    | None -> None
    | Some node -> Some (to_general node |> Soup.children |> Soup.count)

  let get_heading_level node =
    let* node = node in
    if not (is_element node) then None else
    let node = to_element node in
    if not (Html_utils.is_heading node) then None else
    Some (Html_utils.get_heading_level node)
  
  let tname = "html"
  let eq _ = fun x y -> Soup.equal_modulo_whitespace (to_general x) (to_general y)
  let to_string _ s = Soup.to_string (to_general s)
end

module T =
  Lua.Lib.Combine.T2 (Luaiolib.T) (Html)

module LuaioT = T.TV1
module HtmlT  = T.TV2

module MakeLib
  (HtmlV: Lua.Lib.TYPEVIEW with type 'a t = 'a Html.t) :
  Lua.Lib.USERCODE with type 'a userdata' = 'a HtmlV.combined =
struct
  type 'a userdata' = 'a HtmlV.combined
  module M (C: Lua.Lib.CORE with type 'a V.userdata' = 'a userdata') = struct
    module V = C.V
    let ( **-> ) = V.( **-> )
    let ( **->> ) x y = x **-> V.result y
    module Map = struct
      let html = HtmlV.makemap V.userdata V.projection
    end (* Map *)

    (* For ordered iteration. *)
    let get_hash_keys h = V.Luahash.fold (fun k _ acc -> k :: acc) h [] |> List.sort_uniq compare

    (* Move an item from one hash to another, used as an auxiliary function. *)
    let move_item old_hash new_hash key =
      let item = V.Luahash.find old_hash key in
      V.Luahash.add new_hash key item;
      V.Luahash.remove old_hash key

    (* Take up to N keys from a hash and move them to a new hash *)
    let hash_take hash limit =
      let keys = get_hash_keys hash in
      let head_keys = CCList.take limit keys in
      let new_hash = V.Luahash.create 100 in
      let () = List.iter (move_item hash new_hash) head_keys in
      new_hash

    let hash_chunks hash limit =
      let take_chunk old_hash acc keys =
        let new_hash = V.Luahash.create 100 in
        let () = List.iter (move_item old_hash new_hash) keys in
        new_hash :: acc
      in
      let keys = get_hash_keys hash in
      let key_chunks = CCList.chunks limit keys in
      List.fold_left (take_chunk hash) [] key_chunks |> List.rev

    let hash_has_value hash value =
      let exception Found in
      try
        V.Luahash.iter (fun _ v -> if v = value then raise Found) hash;
        (* If we got this far, the item was not found. *)
        false
      with Found -> true

    let hash_find_values p h =
      let add_matching_value h p acc k =
        let item = V.Luahash.find h k in
        if p item then item :: acc
        else acc
      in
      let keys = get_hash_keys h in
      List.fold_left (add_matching_value h p) [] keys |> List.rev

    let hash_get_nested_value h path =
      let rec aux h path =
        match path with
        | [] ->
          (* Normally prevented by the outer function,
             shouldn't happen.
           *)
          internal_error "hash_get_nested_value's inner function got an empty path"
        | [k] -> V.Luahash.find h k
        | k :: ks ->
          try
            let v = V.Luahash.find h k in
            (* If a key exists and its value is a table, look one level deeper. *)
            if V.table.is v then aux (V.table.project v) ks
            (* If a key exists and its value is set to nil, then return nil.
               Since settings a table member to nil in Lua deletes it,
               this case is unlikely, but who knows if it's really impossible in Lua-ML?
             *)
            else if V.unit.is v then v
            (* If a key exists but its value not a table and it's not the last key in the path,
               the table structure clearly doesn't match plugin author's expectations.
               The best thing to do is to throw an error.
             *)
            else plugin_error @@ Printf.sprintf {|value at key "%s" is not a table, cannot look up anything in it|}
              (V.to_string v)
          with Not_found ->
            (* If there is no such key, then the path partially does not exist.
               For the purpose of this function that's fine, it's intended to return a value at a path
               only if every nested subtable exists.
               If not, just return nil to indicate undefined sub-table value.
             *)
            V.unit.embed ()
      in
      match path with
      | [] -> plugin_error "Cannot get a table value at an empty path"
      | _ -> aux h path

    let get_headings_tree soup =
      let open Toc_tree in
      match soup with
      | None -> []
      | Some soup -> begin
        let trees =
          Html_utils.find_headings (Html.to_general soup) |>
          Toc_tree.from_list Html_utils.get_heading_level
        in
        let rec lua_of_tree t =
          let value = Map.html.embed (Html.from_element t.value) in
          match t.children with
          | [] -> ["heading", value; "children", V.unit.embed ()]
          | cs ->
            let children = List.map (fun c -> V.Table.of_list @@ lua_of_tree c) cs in
            let children_table = (V.list V.table).embed children in
            ["heading", value; "children", children_table]
        in List.map (fun t -> lua_of_tree t |> V.Table.of_list |> V.table.embed) trees
      end

    let rec value_of_lua v =
      if V.int.is v then `Float (V.int.project v |> float_of_int)
      (* Lua's float is a supertype of int, so int "is" a float, and the order of checks is important:
         if we checked for float first, the int check would never be executed.
       *)
      else if V.float.is v then `Float (V.float.project v)
      else if V.string.is v then `String (V.string.project v)
      else if V.table.is v then project_lua_table v
      else if V.unit.is v then `Null
      (* Everything in Lua has a truth value, so V.bool.is appears to never fail. *)
      else if V.bool.is v then `Bool (V.bool.project v)
      (* Not sure if this can actually happen but better have a distinctive error if it does. *)
      else internal_error  "Unimplemented Lua to OCaml value projection"
    and project_lua_table t =
      let ts = t |> V.table.project |> V.Luahash.to_seq |> List.of_seq in
      (* In Lua, everything is a table. There are no arrays/lists, only int-indexed tables.
         However, many things, like nav_path, are logically lists. They are lists before we pass them to Lua,
         and it would be nice to have them come back as lists.

         This is why we apply a funny heuristic here: if all keys are integers, we ignore the keys
         and produce a list. This way embedding OCaml lists is reversible, and users who create
         such tables by hand, hopefully, aren't surprised.
       *)
      let keys = CCList.Assoc.keys ts in
      if List.for_all V.int.is keys then
        (* It's an int-indexed table -- a "list".
           However, Hashtbl.to_seq doesn't know about its intended order,
           so we need to sort it by keys ourselves.
         *)
        let ts = List.sort (fun (k, _) (k', _) -> compare k k') ts in
        `A (CCList.Assoc.values ts |> List.map value_of_lua)
      else
         (* Just a normal table, or perhaps a messed-up list... *)
        `O (List.map (fun (k, v) -> (string_of_lua k, value_of_lua v)) ts)
    and string_of_lua v =
      let v' = value_of_lua v in
      match v' with
      | `Float f -> string_of_float f
      | `String s -> s
      | _ ->
        plugin_error @@ Printf.sprintf "Wrong type for a table key: only int, float, and string are supported but %s found"
          (V.to_string v)

    let rec lua_of_json v =
      match v with
      | `Bool b -> V.bool.embed b
      | `Int i -> V.int.embed i
      | `Float f -> V.float.embed f
      | `String s -> V.string.embed s
      | `A vs -> (List.map lua_of_json vs) |> (V.list V.value).embed
      | `O vs ->
        List.map (fun (k, v) -> (k, lua_of_json v)) vs |>
        V.Table.of_list |> V.table.embed
      | `Null -> V.unit.embed ()

    (* For the JSON module. *)

    let parse_json js =
      try Ezjsonm.from_string js |> lua_of_json
      with
      | Ezjsonm.Parse_error (_, err) -> Printf.ksprintf plugin_error "JSON.from_string parse error: %s" err
      | Assert_failure (err, line, pos) -> Printf.ksprintf plugin_error "JSON.from_string internal error: %s:%d%d" err line pos

    let parse_json_unsafe js =
      try Some (parse_json js)
      with Plugin_error err ->
        let () = Logs.warn @@ fun m -> m "JSON.from_string_unsafe failed to parse JSON, returning nil: %s" err in
        let () = Logs.debug @@ fun m -> m "JSON string was:\n %s" js in
        None

    let print_json ?(minify=true) j =
      (* ezjsonm erroneously believes a naked primitive value is not a valid JSON *)
      match j with
      | `O _ as j -> Ezjsonm.to_string ~minify:minify j
      | `A _ as j -> Ezjsonm.to_string ~minify:minify j
      | _ as je -> Utils.string_of_json_primitive je

    (* For the TOML module. *)
    let parse_toml ts =
      let res = Otoml.Parser.from_string_result ts in
      match res with
      | Ok t -> t |> Utils.toml_to_json |> lua_of_json
      | Error err -> Printf.ksprintf plugin_error "TOML.from_string parse error: %s" err

    let parse_toml_unsafe ts =
      let res = Otoml.Parser.from_string_result ts in
      match res with
      | Ok t ->
        let l = t |> Utils.toml_to_json |> lua_of_json in
        Some l
      | Error err ->
        let () = Logs.warn @@ fun m -> m "TOML.from_string_unsafe failed to parse the TOML string, returning nil: %s" err in
        let () = Logs.debug @@ fun m -> m "TOML string was:\n %s" ts in
        None

    (* For the YAML module. *)

    let parse_yaml ys =
      try Yaml.of_string_exn ys |> lua_of_json
      with Invalid_argument err -> Printf.ksprintf plugin_error "YAML.from_string parse error: %s" err

    let parse_yaml_unsafe ys =
      try Some (Yaml.of_string_exn ys |> lua_of_json)
      with Invalid_argument err ->
        let () = Logs.warn @@ fun m -> m "YAML.from_string_unsafe failed to parse the YAML string, returning nil: %s" err in
        let () = Logs.debug @@ fun m -> m "YAML string was:\n %s" ys in
        None

    (* For the Base64 module. *)

    let base64_decode s =
      try Some (Base64.decode_exn s)
      with Invalid_argument msg ->
        let () = Logs.warn @@ fun m -> m "Could not decode a Base64 string: %s" msg in
        None

    let base64_encode s =
      try Some (Base64.encode_exn s)
      with Invalid_argument msg ->
        let () = Logs.warn @@ fun m -> m "Could not encode a Base64 string: %s" msg in
        None

    let url_encode s exclude_chars =
      let chars_of_strings ss =
        try List.map (fun s -> if ((String.length s) = 1) then s.[0]
                               else failwith @@ Printf.sprintf {|String "%s" does not represent a character|} s) ss
        with Failure msg -> plugin_error @@ Printf.sprintf "String.url_encode got an incorrect chars argument: %s" msg
      in
      let exclude_chars = Option.bind exclude_chars (fun ss -> Some (chars_of_strings ss)) in
      Text.url_encode ~exclude_chars:exclude_chars s

    let render_template tmpl data =
      let tmpl = Template.of_string tmpl in
      if not (V.table.is data) then plugin_error "String.render_template requires a table" else
      let data = project_lua_table data in
      match data with
      | `O vs -> List.map (fun (k, v) -> (k, Template.jingoo_of_json v)) vs |> Template.render tmpl
      | `A _ -> plugin_error "String.render_template requires a string-indexed table, found a number-indexed array"
      | _ -> internal_error "project_lua_table returned an unexpected result"

    (* Datetime helpers *)
    let reformat_date date_string input_formats output_format =
      let (>>=) = Stdlib.Option.bind in
      Utils.parse_date input_formats date_string >>=
      (fun d -> Some (Utils.format_date output_format d))

    let to_timestamp date_string input_formats =
      let (>>=) = Stdlib.Option.bind in
      Utils.parse_date input_formats date_string >>=
      (fun d -> Some (ODate.Unix.To.seconds d))

    let current_date output_format =
      let now = ODate.Unix.now () in
      Utils.format_date output_format now

    let init g = 
      C.register_module "HTML" [
        "mk", V.efunc (V.unit **->> Map.html) (fun () -> Html.SoupNode (Soup.create_soup ()));
        "create_document", V.efunc (V.unit **->> Map.html) (fun () -> Html.SoupNode (Soup.create_soup ()));
        "create_element", V.efunc (V.string **-> V.option V.string **->> Map.html) Html.create_element;
        "create_text", V.efunc (V.string **->> Map.html) Html.create_text;
        "parse", V.efunc (V.string **->> Map.html) (fun s -> Html.SoupNode (Soup.parse s));
        "to_string", V.efunc (Map.html **->> V.string) (fun s -> Html.to_general s |> Soup.to_string);
        "pretty_print", V.efunc (Map.html **->> V.string) (fun s -> Html.to_general s |> Soup.pretty_print);
        "select", V.efunc (V.option Map.html **-> V.string **->> V.option (V.list Map.html)) Html.select;
        "select_one", V.efunc (V.option Map.html **-> V.string **->> (V.option Map.html)) Html.select_one;
        "select_any_of", V.efunc (V.option Map.html **-> V.list V.string **->> V.option Map.html) Html.select_any_of;
        "select_all_of", V.efunc (V.option Map.html **-> V.list V.string **->> V.option (V.list Map.html)) Html.select_all_of;
        "matches_selector", V.efunc (Map.html **-> Map.html **-> V.string **->> V.bool) Html.matches_selector;
        "matches_any_of_selectors", V.efunc (Map.html **-> Map.html **-> V.list V.string **->> V.bool) Html.matches_any_of_selectors;
        "parent", V.efunc (V.option Map.html **->> (V.option Map.html)) Html.parent;
        "children", V.efunc (V.option Map.html **->> V.option (V.list Map.html)) Html.children;
        "descendants", V.efunc (V.option Map.html **->> V.option (V.list Map.html)) Html.descendants;
        "ancestors", V.efunc (V.option Map.html **->> V.option (V.list Map.html)) Html.ancestors;
        "siblings", V.efunc (V.option Map.html **->> V.option (V.list Map.html)) Html.siblings;
        "set_tag_name", V.efunc (V.option Map.html **-> V.string **->> V.unit) Html.set_tag_name;
        "get_tag_name", V.efunc (V.option Map.html **->> V.option V.string) Html.get_tag_name;
        "get_attribute", V.efunc (V.option Map.html **-> V.string **->> V.option V.string) Html.get_attribute;
        "set_attribute", V.efunc (V.option Map.html **-> V.string **-> V.string **->> V.unit) Html.set_attribute;
        "append_attribute", V.efunc (V.option Map.html **-> V.string **-> V.string **->> V.unit) Html.append_attribute;
        "delete_attribute", V.efunc (V.option Map.html **-> V.string **->> V.unit) Html.delete_attribute;
        "list_attributes", V.efunc (V.option Map.html **->> V.list V.string) Html.list_attributes;
        "clear_attributes", V.efunc (V.option Map.html **->> V.unit) Html.clear_attributes;
        "add_class", V.efunc (V.option Map.html **-> V.string **->> V.unit) Html.add_class;
        "remove_class", V.efunc (V.option Map.html **-> V.string **->> V.unit) Html.remove_class;
        "get_classes", V.efunc (V.option Map.html **->> V.list V.string) Html.get_classes;
        "has_class", V.efunc (V.option Map.html **-> V.string **->> V.bool) Html.has_class;
        "append_child", V.efunc (V.option Map.html **-> Map.html **->> V.unit) (Html.do_with_node Html.append_child);
        "prepend_child", V.efunc (V.option Map.html **-> Map.html **->> V.unit) (Html.do_with_node Html.prepend_child);
        "insert_before", V.efunc (V.option Map.html **-> Map.html **->> V.unit) (Html.do_with_node Html.insert_before);
        "insert_after", V.efunc (V.option Map.html **-> Map.html **->> V.unit) (Html.do_with_node Html.insert_after);
        "replace", V.efunc (V.option Map.html **-> Map.html **->> V.unit) (Html.do_with_node Html.replace);
        "replace_element", V.efunc (V.option Map.html **-> Map.html **->> V.unit) (Html.do_with_node Html.replace);
        "replace_content", V.efunc (V.option Map.html **-> Map.html **->> V.unit) (Html.do_with_node Html.replace_content);
        "delete_content", V.efunc (V.option Map.html **->> V.unit) Html.delete_content;
        "delete", V.efunc (V.option Map.html **->> V.unit) Html.delete;
        "delete_element", V.efunc (V.option Map.html **->> V.unit) Html.delete;
        "inner_html", V.efunc (V.option Map.html **->> V.string) Html.inner_html;
        "clone_content", V.efunc (V.option Map.html **->> V.option Map.html) Html.clone_content;
        "clone_document", V.efunc (Map.html **->> Map.html) Html.clone_page;
        "strip_tags", V.efunc (V.option Map.html **->> V.string) Html.strip_tags;
        "inner_text", V.efunc (V.option Map.html **->> V.string) Html.strip_tags;
        "append_root", V.efunc (Map.html **-> Map.html **->> V.unit) Html.append_root;
        "prepend_root", V.efunc (Map.html **-> Map.html **->> V.unit) Html.prepend_root;
        "unwrap", V.efunc (V.option Map.html **->> V.unit) Html.unwrap;
        "child_count", V.efunc (V.option Map.html **->> V.option V.int) Html.child_count;
        "is_element", V.efunc (V.option Map.html **->> V.bool) (fun n -> match n with None -> false | Some n -> Html.is_element n);
        "get_headings_tree", V.efunc (V.option Map.html **->> V.list V.value) get_headings_tree;
        "get_heading_level", V.efunc (V.option Map.html **->> V.option V.int) Html.get_heading_level;
      ] g;
      
      C.register_module "Regex" [
        "replace", V.efunc (V.string **-> V.string **-> V.string **->> V.string)
          (Re_wrapper.replace ~all:false);
        "replace_all", V.efunc (V.string **-> V.string **-> V.string **->> V.string)
          (Re_wrapper.replace_all);
        "find_all", V.efunc (V.string **-> V.string **->> (V.list V.string)) Re_wrapper.find_all;
        "match", V.efunc (V.string **-> V.string **->> V.bool) Re_wrapper.re_match;
        "split", V.efunc (V.string **-> V.string **->> (V.list V.string)) Re_wrapper.split;
      ] g;

      C.register_module "Log" [
        "debug", V.efunc (V.string **->> V.unit) Log.debug;
        "info", V.efunc (V.string **->> V.unit) Log.info;
        "warning", V.efunc (V.string **->> V.unit) Log.warning;
        "error", V.efunc (V.string **->> V.unit) Log.error
      ] g;

     C.register_module "Plugin" [
       "fail", V.efunc (V.string **->> V.unit) (fun s -> plugin_error s);
       "exit", V.efunc (V.option V.string **->> V.unit) (fun e -> raise (Plugin_exit e));
       "require_version", V.efunc (V.string **->> V.unit) Plugin_version.require_version;
       "soupault_version", V.efunc (V.unit **->> V.string) (fun () -> Defaults.version_string);
     ] g;

     C.register_module "Sys" [
       (* File operations. *)
       "mkdir", V.efunc (V.string **->> V.unit) (fun d -> try FileUtil.mkdir ~parent:true d with FileUtil.MkdirError msg -> plugin_error msg);
       "read_file", V.efunc (V.string **->> V.option V.string) (Sys_wrappers.read_file);
       "write_file", V.efunc (V.string **-> V.string **->> V.unit) (Sys_wrappers.write_file);
       "get_file_size", V.efunc (V.string **->> V.option V.int) (fun s -> try Some (Unix.stat s).st_size with _ -> None);
       "get_file_modification_time", V.efunc (V.string **->> V.option V.float) (fun s -> try Some (Unix.stat s).st_mtime with _ -> None);
       "file_exists", V.efunc (V.string **->> V.bool) (fun s -> FileUtil.test FileUtil.Exists s);
       "is_file", V.efunc (V.string **->> V.bool) (fun s -> FileUtil.test FileUtil.Is_file s);
       "is_dir", V.efunc (V.string **->> V.bool) (fun s -> FileUtil.test FileUtil.Is_dir s);
       "delete_file", V.efunc (V.string **->> V.unit) Sys_wrappers.delete_file;
       "delete_recursive", V.efunc (V.string **->> V.unit) (fun s -> Sys_wrappers.delete_file ~r:true s);
       "list_dir", V.efunc (V.string **->> V.list V.string) Sys_wrappers.ls;
       (* External program execution *)
       "get_program_output", V.efunc (V.string **-> V.option V.string **->> V.option V.string) (Sys_wrappers.get_program_output);
       "run_program", V.efunc (V.string **->> V.option V.int) (Sys_wrappers.run_program);
       "run_program_get_exit_code", V.efunc (V.string **->> V.int) (Sys_wrappers.run_program_get_exit_code);
       (* Operations on native file paths. *)
       "join_path", V.efunc (V.string **-> V.string **->> V.string) FilePath.concat;
       "split_path", V.efunc (V.string **->> V.list V.string) File_path.split_path;
       "basename", V.efunc (V.string **->> V.string) FilePath.basename;
       "dirname", V.efunc (V.string **->> V.string) FilePath.dirname;
       (* Operations on UNIX/URL paths. *)
       "join_path_unix", V.efunc (V.string **-> V.string **->> V.string) FilePath.UnixPath.concat;
       "join_url", V.efunc (V.string **-> V.string **->> V.string) FilePath.UnixPath.concat;
       "split_path_unix", V.efunc (V.string **->> V.list V.string) File_path.split_path_unix;
       "split_path_url", V.efunc (V.string **->> V.list V.string) File_path.split_path_unix;
       "basename_unix", V.efunc (V.string **->> V.string) FilePath.UnixPath.basename;
       "dirname_unix", V.efunc (V.string **->> V.string) FilePath.UnixPath.dirname;
        (* Operations with file extensions. *)
       "get_extension", V.efunc (V.string **->> V.string) Sys_wrappers.get_extension;
       "get_extensions", V.efunc (V.string **->> V.list V.string) Sys_wrappers.get_extensions;
       "has_extension", V.efunc (V.string **-> V.string **->> V.bool) (fun f e -> Sys_wrappers.has_extension e f);
       "strip_extensions", V.efunc (V.string **->> V.string) Sys_wrappers.strip_extensions;
       (* Misc. *)
       "random", V.efunc (V.int **->> V.int) Random.int;
       "is_windows", V.efunc (V.unit **->> V.bool) (fun () -> Sys.win32);
       "is_unix", V.efunc (V.unit **->> V.bool) (fun () -> Sys.unix);
       "getenv", V.efunc (V.string **-> V.option V.string **->> V.option V.string) Sys_wrappers.getenv;
     ] g;

     C.register_module "String" [
       "trim", V.efunc (V.string **->> V.string) String.trim;
       "length", V.efunc (V.string **->> V.int) Text.length;
       "length_ascii", V.efunc (V.string **->> V.int) String.length;
       "truncate", V.efunc (V.string **-> V.int **->> V.string)
         (fun s l -> try Text.sub s 0 l with Invalid_argument _ -> s);
       "truncate_ascii", V.efunc (V.string **-> V.int **->> V.string)
         (fun s l -> try String.sub s 0 l with Invalid_argument _ -> s);
       "slugify_soft", V.efunc (V.string **->> V.string) (fun s -> Regex_utils.Internal.replace ~regex:{|\s+|} ~sub:"-" s);
       "slugify_ascii", V.efunc (V.string **->> V.string) Utils.slugify;
       "join", V.efunc (V.string **-> V.list V.string **->> V.string) String.concat;
       "to_number", V.efunc (V.string **->> V.option V.float) (fun s -> try Some (float_of_string s) with _ -> None);
       "render_template", V.efunc (V.string **-> V.value **->> V.string) render_template;
       "base64_encode", V.efunc (V.string **->> V.option V.string) base64_encode;
       "base64_decode", V.efunc (V.string **->> V.option V.string) base64_decode;
       "url_encode", V.efunc (V.string **-> V.option (V.list V.string) **->> V.string) url_encode;
       "url_decode", V.efunc (V.string **->> V.string) Text.url_decode;
       "starts_with", V.efunc (V.string **-> V.string **->> V.bool) (fun s pat -> String.starts_with ~prefix:pat s);
       "ends_with", V.efunc (V.string **-> V.string **->> V.bool) (fun s pat -> String.ends_with ~suffix:pat s);
       "is_valid_utf8", V.efunc (V.string **->> V.bool) Text.is_valid_utf8;
       "is_valid_ascii", V.efunc (V.string **->> V.bool) Text.is_valid_ascii;
     ] g;

    C.register_module "JSON" [
      "from_string", V.efunc (V.string **->> V.value) parse_json;
      "unsafe_from_string", V.efunc (V.string **->> V.option V.value) parse_json_unsafe;
      "to_string", V.efunc (V.value **->> V.string) (fun v -> value_of_lua v |> print_json);
      "pretty_print", V.efunc (V.value **->> V.string) (fun v -> value_of_lua v |> print_json ~minify:false);
    ] g;

    C.register_module "YAML" [
      "from_string", V.efunc (V.string **->> V.value) parse_yaml;
      "unsafe_from_string", V.efunc (V.string **->> V.option V.value) parse_yaml_unsafe;
      (* We don't provide a to_string function for YAML
         because YAML parsing into Lua is done with type information loss. *)
    ] g;

    C.register_module "TOML" [
      "from_string", V.efunc (V.string **->> V.value) parse_toml;
      "unsafe_from_string", V.efunc (V.string **->> V.option V.value) parse_toml_unsafe;
      (* We don't provide printing for TOML either
         because type information loss between TOML and Lua is even worse than with YAML. *)
    ] g;

    C.register_module "Date" [
      "reformat", V.efunc (V.string **-> V.list V.string **-> V.string **->> V.option V.string) reformat_date;
      "to_timestamp", V.efunc (V.string **-> V.list V.string **->> V.option V.int) to_timestamp;
      "now_format", V.efunc (V.string **->> V.string) current_date;
      "now_timestamp", V.efunc (V.unit **->> V.int) (fun () -> ODate.Unix.now () |> ODate.Unix.To.seconds);
    ] g;

    C.register_module "Table" [
      "has_key", V.efunc (V.table **-> V.value **->> V.bool) (fun t k -> V.Luahash.find_opt t k |> Option.is_some);
      "has_value", V.efunc (V.table **-> V.value **->> V.bool) hash_has_value;
      "get_key_default", V.efunc (V.table **-> V.value **-> V.value **->> V.value) (fun t k d -> V.Luahash.find_opt t k |> Option.value ~default:d);
      "get_nested_value", V.efunc (V.table **-> V.list V.value **->> V.value) hash_get_nested_value;
      "get_nested_value_default", V.efunc (V.table **-> V.list V.value **-> V.value **->> V.value)
        (fun t p d -> let v = hash_get_nested_value t p in if V.unit.is v then d else v);
      (* Reasons to have these iteration primitives include not just convenience,
         but also the fact that for-loops in Lua can't handle lists with gaps in item numbering.
         These functions handle it just fine since they just iterate through all keys.
       *)
      "iter", V.efunc ((V.func (V.value **-> V.value **->> V.unit)) **-> V.table **->> V.unit) V.Luahash.iter;
      "iter_values", V.efunc ((V.func (V.value **->> V.unit)) **-> V.table **->> V.unit) (fun f t -> V.Luahash.iter (fun _ v -> f v) t);
      "iter_ordered", V.efunc ((V.func (V.value **-> V.value **->> V.unit)) **-> V.table **->> V.unit)
        (fun f h -> List.iter (fun k -> f k (V.Luahash.find h k)) @@ get_hash_keys h);
      "iter_values_ordered", V.efunc ((V.func (V.value **->> V.unit)) **-> V.table **->> V.unit)
        (fun f h -> List.iter (fun k -> f (V.Luahash.find h k)) @@ get_hash_keys h);
      "apply", V.efunc ((V.func (V.value **-> V.value **->> V.option V.value)) **-> V.table **->> V.unit) V.Luahash.filter_map_inplace;
      "apply_to_values", V.efunc ((V.func (V.value **->> V.option V.value)) **-> V.table **->> V.unit)
        (fun f h -> V.Luahash.filter_map_inplace (fun _ v -> f v) h);
      "fold", V.efunc ((V.func (V.value **-> V.value **-> V.value **->> V.value)) **-> V.table **-> V.value **->> V.value) V.Luahash.fold;
      "fold_values", V.efunc ((V.func (V.value **-> V.value **->> V.value)) **-> V.table **-> V.value **->> V.value)
        (fun f t i -> V.Luahash.fold (fun _ v acc -> f v acc) t i);
      "find_values", V.efunc ((V.func (V.value **->> V.bool)) **-> V.table **->> V.list V.value) hash_find_values;
      "take", V.efunc (V.table **-> V.int **->> V.table) hash_take;
      "chunks", V.efunc (V.table **-> V.int **->> V.list V.table) hash_chunks;
      "keys", V.efunc (V.table **->> V.list V.value) get_hash_keys;
    ] g;

    C.register_module "Value" [
      (* Lua(-ML) refuses to print certain values like nil,
         repr makes it safe to pass values to printers and formatters. *)
      "repr", V.efunc (V.value **->> V.string) V.to_string;
      (* Lua officially only has a single number type.
         However, in Lua-ML integers and floats are distinct,
         so we can offer the user a way to tell them apart. *)
      "is_int", V.efunc (V.value **->> V.bool) V.int.is;
      "is_float", V.efunc (V.value **->> V.bool) V.float.is;
      "is_string", V.efunc (V.value **->> V.bool) V.string.is;
      (* There are no true, ordered arrays in Lua,
         so they have to be fakes with number-indexed tables
         and ordered iteration methods.
         We give the user a way to find out if something is a table (of any kind)
         or an integer-indexed table that can be handled as a list. *)
      "is_table", V.efunc (V.value **->> V.bool) V.table.is;
      "is_list", V.efunc (V.value **->> V.bool)
        (* A table is considered a list if all its keys are numeric. *)
        (fun t -> if not (V.table.is t) then false
                  else t |> V.table.project |> V.Luahash.to_seq |> List.of_seq |>
                            CCList.Assoc.keys |> List.for_all V.int.is);
      "is_nil", V.efunc (V.value **->> V.bool) V.unit.is;
    ] g;
  end (* M *)
end (* MakeLib *)

module W = Lua.Lib.WithType (T)
module C  =
    Lua.Lib.Combine.C5  (* C5 == combine 5 code modules *)
        (Luaiolib.Make(LuaioT))
        (Luacamllib.Make(LuaioT))
        (W (Luastrlib.M))
        (W (Luamathlib.M))
        (MakeLib (HtmlT))


module I = (* interpreter *)  
    Lua.MakeInterp
        (Lua.Parser.MakeStandard)
        (Lua.MakeEval (T) (C))

let lua_of_soup s =
  let v = HtmlT.makemap I.Value.userdata I.Value.projection in
  v.embed s

let soup_of_lua l =
  let v = HtmlT.makemap I.Value.userdata I.Value.projection in
  v.project l

let rec lua_of_json v =
  match v with
  | `Bool b -> I.Value.bool.embed b
  | `Int i -> I.Value.int.embed i
  | `Float f -> I.Value.float.embed f
  | `String s -> I.Value.string.embed s
  | `A vs -> (List.map lua_of_json vs) |> (I.Value.list I.Value.value).embed
  | `O vs ->
    List.map (fun (k, v) -> (k, lua_of_json v)) vs |>
    I.Value.Table.of_list |> I.Value.table.embed
  | `Null -> I.Value.unit.embed ()

let rec lua_of_toml v =
  let open Otoml in
  match v with
  | TomlBoolean b -> I.Value.bool.embed b
  | TomlInteger i -> I.Value.int.embed i
  | TomlFloat f -> I.Value.float.embed f
  | TomlString s -> I.Value.string.embed s
  | TomlArray vs | TomlTableArray vs -> (List.map lua_of_toml vs) |> (I.Value.list I.Value.value).embed
  | TomlTable vs | TomlInlineTable vs  ->
    List.map (fun (k, v) -> (k, lua_of_toml v)) vs |>
    I.Value.Table.of_list |> I.Value.table.embed
  | TomlLocalTime s -> I.Value.string.embed s
  | TomlLocalDate s -> I.Value.string.embed s
  | TomlLocalDateTime s -> I.Value.string.embed s
  | TomlOffsetDateTime s -> I.Value.string.embed s

(* If you are wondering if fhis is a duplicate of the value_of_lua function found inside the MakeLib module,
   then yes, effectively it is.
   The problem is that MakeLib functions are not visible outside that module,
   but we can't define this function before the Value module is created,
   since it refers to functions from it, so we've got what we've got.
   Luckily, this code is very rarely modified, so duplication isn't a terribly big problem. 
 *)
let json_of_lua v =
  let module V = I.Value in
  let rec project_lua_value v =
    if V.int.is v then `Float (V.int.project v |> float_of_int)
    (* Lua's float is a supertype of int, so every int is also a float, and the order of checks is important:
       if we checked for float first, the int check would never be executed. 
     *)
    else if V.float.is v then `Float (V.float.project v)
    else if V.string.is v then `String (V.string.project v)
    else if V.table.is v then project_lua_table v
    else if V.unit.is v then `Null
    (* Everything in Lua has a truth value, so V.bool.is appears to never fail *)
    else if V.bool.is v then `Bool (V.bool.project v)
    (* Not sure if this actually can happen *)
    else internal_error "Unimplemented Lua to OCaml value projection"
  and project_lua_table t =
    let ts = t |> V.table.project |> V.Luahash.to_seq |> List.of_seq in
    (* In Lua, everything is a table. There are no arrays/lists, only int-indexed tables.
       However, many things, like nav_path, are logically lists. They are lists before we pass them to Lua,
       and it would be nice to have them come back as lists.

       This is why we apply a funny heuristic here: if all keys are integers, we ignore the keys
       and produce a list. This way embedding OCaml lists is reversible, and users who create
       such tables by hand, hopefully, aren't surprised.
     *)
    let keys = CCList.Assoc.keys ts in
    if List.for_all V.int.is keys then
      (* It's an int-indexed table -- a "list".
         However, Hashtbl.to_seq doesn't know about its intended order,
         so we need to sort it by keys ourselves.
       *)
      let ts = List.sort (fun (k, _) (k', _) -> compare k k') ts in
      `A (CCList.Assoc.values ts |> List.map project_lua_value)
    else
       (* Just a normal table, or perhaps a messed-up list... *)
      `O (List.map (fun (k, v) -> (string_of_lua k, project_lua_value v)) ts)
  and string_of_lua v =
    let v' = project_lua_value v in
    match v' with
    | `Float f -> string_of_float f
    | `String s -> s
    | _ ->
      plugin_error @@ Printf.sprintf "Wrong type for a table key: only int, float, and string are supported but %s found"
        (V.to_string v)
  in
  try Ok (project_lua_value v)
  with Plugin_error msg -> Error msg

let get_global state name vmap =
  let value = I.getglobal state (I.Value.string.embed name) in
  if not (I.Value.(vmap.is) value) then Error (Printf.sprintf "wrong Lua type for variable %s" name)
  else Ok (I.Value.(vmap.project) value)

let make_plugin_env () = ref (I.Value.Table.of_list [] |> I.Value.table.embed)

let run_lua filename state lua_code =
  try
    let _ = I.dostring ~file:filename state lua_code in
    Ok ()
  with
  | Failure msg -> Error (Printf.sprintf "Lua code execution failed:\n%s" msg)
  | Plugin_error msg -> Error msg
  | Luascanner.Scan msg -> Error (Printf.sprintf "Lua syntax error: %s" msg)
  | Plugin_exit msgo ->
    begin
      match msgo with
      | Some msg -> let () = Logs.info @@ fun m -> m "Plugin exited with message: %s" msg in Ok ()
      | None -> Ok ()
    end

let run_plugin settings soupault_config filename lua_code plugin_env_ref env widget_config soup =
  let open Defaults in
  let lua_str_list = I.Value.list I.Value.string in
  let lua_str = I.Value.string in
  let plugin_env = !plugin_env_ref in
  let state = I.mk () in
  let () =
    (* Set up the built-in plugin environment *)
    I.register_globals [
      "page", lua_of_soup (Html.SoupNode soup);
      "nav_path", lua_str_list.embed env.nav_path;
      "page_file", lua_str.embed env.page_file;
      "page_url", lua_str.embed env.page_url;
      "target_dir", lua_str.embed env.target_dir;
      "target_file", lua_str.embed env.target_file;
      "site_index", lua_of_json (Utils.json_of_index_entries env.site_index);
      "config", lua_of_toml widget_config;
      "widget_config", lua_of_toml widget_config;
      "soupault_config", lua_of_toml soupault_config;
      "force", I.Value.bool.embed settings.force;
      "build_dir", lua_str.embed settings.build_dir;
      "site_dir", lua_str.embed settings.site_dir;
      (* Restore the persistent data from previous plugin runs *)
      "persistent_data", plugin_env
    ] state
  in
  run_lua filename state lua_code
