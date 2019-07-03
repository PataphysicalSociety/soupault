type 'a widget = {
  name : string;
  config: TomlTypes.table;
  func: TomlTypes.table -> 'a Soup.node -> (unit, string) result
}

(* Option monad *)
let (>>=) = CCOpt.(>>=)

(* Quick and dirty widget lookup *)
let find_widget name =
  try
    Some (List.assoc name Builtin_widgets.widgets)
  with Not_found -> None


(* Widget config loading *)
let get_widget_config config widget =
  let widget_tbl = Config.get_table Defaults.widgets_table config >>= Config.get_table widget in
  match widget_tbl with
  | Some widget_tbl -> widget_tbl
  | None ->
    (* This function is, or should be used only with widget names already
       retrieved from the config *)
   failwith @@ Printf.sprintf "Trying to lookup a non-existent widget %s" widget

let list_widgets config =
  let (>>=) = CCOpt.(>>=) in
  let ws = Config.get_table Defaults.widgets_table config >>= (fun x -> Some (Config.list_config_keys x)) in
  match ws with
  | None -> []
  | Some ws' -> ws'

(* The real widget loading function *)
let rec _load_widgets config ws =
  match ws with
  | [] -> []
  | w :: ws' ->
    let widget_config = get_widget_config config w in
    let name = Config.get_string "widget" widget_config in
    begin
      match name with
      | None -> failwith (Printf.sprintf "In [widgets.%s]: missing required option widget=\"<some widget>\"" w)
      | Some n ->
        let widget_func = find_widget n in
        begin
          match widget_func with
          | None -> failwith (Printf.sprintf "In [widgets.%s]: unknown widget \"%s\"" w n)
          | Some wf ->
            let widget_rec = {name=n; config=widget_config; func=wf} in
            widget_rec :: (_load_widgets config ws')
        end
    end

(* The monadic wrapping for it *)
let load_widgets config =
  let ws = list_widgets config in
  try Ok (_load_widgets config ws)
  with Failure msg -> Error msg



(* Built-in widgets *)
let set_title config soup =
  (* Retrieve config options. The "selector" option means title source element, by default the first <h1> *)
  let selector = Config.get_string_default "h1" "selector" config in
  let prepend = Config.get_string_default "" "prepend" config in
  let append = Config.get_string_default "" "append" config in
  let default_title = Config.get_string_default "" "default" config in
  (* Now to setting the title *)
  let title_node = Soup.select_one "title" soup in
  match title_node with
  | None ->
    let () = Logs.info @@ fun m -> m "Page has no <title> node, assuming you don't want to set it" in
    Ok ()
  | Some title_node ->
    let title_string = Soup.select_one selector soup >>= Soup.leaf_text |> Config.default default_title in
    let title_string = Printf.sprintf "%s%s" prepend title_string in
    let title_string = Printf.sprintf "%s%s" title_string append in
    (* XXX: Both Soup.create_text and Soup.create_element ~inner_text:... escape special characters
       instead of expanding entities, so "&mdash;" becomes "&amp;mdash", which is not what we want.
       Soup.parse expands them, which is why it's used here *)
    let new_title_node = Printf.sprintf "<title>%s</title>" title_string |> Soup.parse in
    let () = Soup.replace title_node new_title_node in
    Ok ()


