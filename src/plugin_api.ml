exception Plugin_error of string
exception Plugin_exit of string option

module Re_wrapper = struct
  let replace ?(all=false) s pat sub =
    try
      let re = Re.Perl.compile_pat pat in
      Re.replace ~all:all ~f:(fun _ -> sub) re s
    with Re__Perl.Parse_error | Re__Perl.Not_supported ->
      raise (Plugin_error (Printf.sprintf "Malformed regex \"%s\"" pat))

  let find_all s pat =
    try
      let re = Re.Perl.compile_pat pat in
      Re.matches re s
    with Re__Perl.Parse_error | Re__Perl.Not_supported->
      raise (Plugin_error (Printf.sprintf "Malformed regex \"%s\"" pat))

  let re_match s pat =
    let ms = find_all s pat in
    List.length ms != 0

  let split s pat =
    try
      Re.split (Re.Perl.compile_pat pat) s
    with Re__Perl.Parse_error | Re__Perl.Not_supported->
      raise (Plugin_error (Printf.sprintf "Malformed regex \"%s\"" pat))
end

module Log = struct
  let info s = Logs.info @@ fun m -> m "%s" s
  let warning s = Logs.warn @@ fun m -> m "%s" s
  let error s = Logs.err @@ fun m -> m "%s" s
end

module Sys_wrappers = struct
  let read_file name =
    try Some (Soup.read_file name)
    with
    | Sys_error msg -> let () = Logs.err @@ fun m -> m "Failed to read file: %s" msg in None
end

module Html = struct
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
    | _ -> raise (Plugin_error "Expected an element, but found a document")

  let to_general n =
    match n with
    | GeneralNode n -> n
    | ElementNode n -> Soup.coerce n
    | SoupNode n -> Soup.coerce n

  let select soup selector =
    to_general soup |> Soup.select selector |> Soup.to_list |> List.map (fun x -> ElementNode x)

  let select_one soup selector =
    let n = to_general soup |> Soup.select_one selector in
    match n with
    | Some n -> Some (ElementNode n)
    | None -> None

  let get_attribute node attr_name =
    to_element node |> Soup.attribute attr_name

  let set_attribute node attr_name attr_value =
    to_element node |> Soup.set_attribute attr_name attr_value

  let add_class node class_name =
    to_element node |> Soup.add_class class_name

  let remove_class node class_name =
    to_element node |> Soup.remove_class class_name

  let append_child node child =
    let child = to_general child in
    match node with
    | ElementNode n -> Soup.append_child n child
    | SoupNode n -> Soup.append_root n child
    | GeneralNode _ -> raise (Plugin_error "Cannot append a child to a GeneralNode")

  let prepend_child node child =
    let child = to_general child in
    match node with
    | ElementNode n -> Soup.prepend_child n child
    | SoupNode _ -> raise (Plugin_error "Cannot prepend a child to a document node")
    | GeneralNode _ -> raise (Plugin_error "Cannot prepend a child to a general node")

  let insert_before node child =
    let child = to_general child in
    match node with
    | ElementNode n -> Soup.insert_before n child
    | SoupNode _ -> raise (Plugin_error "Cannot use insert_before with a document node")
    | GeneralNode _ -> raise (Plugin_error "Cannot user insert_before with a general node")

  let insert_after node child =
    let child = to_general child in
    match node with
    | ElementNode n -> Soup.insert_after n child
    | SoupNode _ -> raise (Plugin_error "Cannot use insert_after with a document node")
    | GeneralNode _ -> raise (Plugin_error "Cannot user insert_after with a general node")

  let replace node child =
    let child = to_general child in
    match node with
    | ElementNode n -> Soup.replace n child
    | SoupNode _ -> raise (Plugin_error "Cannot use replace with a document node")
    | GeneralNode _ -> raise (Plugin_error "Cannot use replace with a general node")

  let replace_content node child =
    let child = to_general child in
    match node with
    | ElementNode n -> Utils.replace_content n child
    | SoupNode _ -> raise (Plugin_error "Cannot use replace_content with a document node")
    | GeneralNode _ -> raise (Plugin_error "Cannot use replace_content with a general node")

  let delete node =
    to_general node |> Soup.delete

  let create_element name text =
    let text = CCOpt.get_or ~default:"" text in
    ElementNode (Soup.create_element ~inner_text:text name)

  let create_text text = GeneralNode (Soup.create_text text)

  let inner_html node =
    to_general node |> Utils.inner_html

  let strip_tags node = to_general node |> Utils.get_element_text |> CCOpt.get_or ~default:""
  
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
   
    let init g = 
      C.register_module "HTML" [
        "mk", V.efunc (V.unit **->> Map.html) (fun () -> Html.SoupNode (Soup.create_soup ()));
        "parse", V.efunc (V.string **->> Map.html) (fun s -> Html.SoupNode (Soup.parse s));
        "select", V.efunc (Map.html **-> V.string **->> (V.list Map.html)) Html.select;
        "select_one", V.efunc (Map.html **-> V.string **->> (V.option Map.html)) Html.select_one;
        "get_attribute", V.efunc (Map.html **-> V.string **->> V.option V.string) Html.get_attribute;
        "set_attribute", V.efunc (Map.html **-> V.string **-> V.string **->> V.unit) Html.set_attribute;
        "add_class", V.efunc (Map.html **-> V.string **->> V.unit) Html.add_class;
        "remove_class", V.efunc (Map.html **-> V.string **->> V.unit) Html.remove_class;
        "append_child", V.efunc (Map.html **-> Map.html **->> V.unit) Html.append_child;
        "prepend_child", V.efunc (Map.html **-> Map.html **->> V.unit) Html.prepend_child;
        "insert_before", V.efunc (Map.html **-> Map.html **->> V.unit) Html.insert_before;
        "insert_after", V.efunc (Map.html **-> Map.html **->> V.unit) Html.insert_after;
        "replace", V.efunc (Map.html **-> Map.html **->> V.unit) Html.replace;
        "replace_element", V.efunc (Map.html **-> Map.html **->> V.unit) Html.replace;
        "replace_content", V.efunc (Map.html **-> Map.html **->> V.unit) Html.replace_content;
        "delete", V.efunc (Map.html **->> V.unit) Html.delete;
        "delete_element", V.efunc (Map.html **->> V.unit) Html.delete;
        "create_element", V.efunc (V.string **-> V.option V.string **->> Map.html) Html.create_element;
        "create_text", V.efunc (V.string **->> Map.html) Html.create_text;
        "inner_html", V.efunc (Map.html **->> V.string) Html.inner_html;
        "strip_tags", V.efunc (Map.html **->> V.string) Html.strip_tags
      ] g;
      
      C.register_module "Regex" [
        "replace", V.efunc (V.string **-> V.string **-> V.string **->> V.string)
          (Re_wrapper.replace ~all:false);
        "replace_all", V.efunc (V.string **-> V.string **-> V.string **->> V.string)
          (Re_wrapper.replace ~all:true);
        "find_all", V.efunc (V.string **-> V.string **->> (V.list V.string)) Re_wrapper.find_all;
        "match", V.efunc (V.string **-> V.string **->> V.bool) Re_wrapper.re_match;
        "split", V.efunc (V.string **-> V.string **->> (V.list V.string)) Re_wrapper.split;
      ] g;

      C.register_module "Log" [
        "info", V.efunc (V.string **->> V.unit) Log.info;
        "warning", V.efunc (V.string **->> V.unit) Log.warning;
        "error", V.efunc (V.string **->> V.unit) Log.error
      ] g;

     C.register_module "Plugin" [
       "fail", V.efunc (V.string **->> V.unit) (fun s -> raise (Plugin_error s));
       "exit", V.efunc (V.option V.string **->> V.unit) (fun e -> raise (Plugin_exit e));
     ] g;

     C.register_module "Sys" [
       "read_file", V.efunc (V.string **->> V.option V.string) (Sys_wrappers.read_file);
       "join_path", V.efunc (V.string **-> V.string **->> V.string) FilePath.concat
     ] g
  end (* M *)
end (* MakeLib *)

module W = Lua.Lib.WithType (T)
module C  =
    Lua.Lib.Combine.C5  (* C5 == combine 4 code modules *)
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

let lua_of_config c =
  let cs = Config.assoc_of_table Config.get_whatever_as_string c in
  let cs = List.map (fun (k, v) -> (k, I.Value.string.embed v)) cs in
  let config_hash = I.Value.Table.of_list cs in
  I.Value.table.embed config_hash

let run_plugin lua_code env config soup =
  let open Defaults in
  let lua_str_list = I.Value.list I.Value.string in
  let lua_str = I.Value.string in
  try
    let state = I.mk () in
    let () =
      I.register_globals ["page", lua_of_soup (Html.SoupNode soup)] state;
      I.register_globals ["nav_path", lua_str_list.embed env.nav_path] state;
      I.register_globals ["page_file", lua_str.embed env.page_file] state;
      I.register_globals ["page_url", lua_str.embed env.page_url] state;
      I.register_globals ["config", lua_of_config config] state
    in
    let _ = I.dostring state lua_code in
    Ok ()
  with
  | Plugin_error msg -> Error msg
  | Plugin_exit msgo ->
    begin
      match msgo with
      | Some msg -> let () = Logs.info @@ fun m -> m "Plugin exited with message: %s" msg in Ok ()
      | None -> Ok ()
    end
