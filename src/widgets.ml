type 'a widget = {
  name : string;
  config: TomlTypes.table;
  func: Defaults.env -> TomlTypes.table -> 'a Soup.node -> (unit, string) result
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
  (* Keep widget order the same as in the config *)
  let ws = List.rev ws in
  try Ok (_load_widgets config ws)
  with Failure msg -> Error msg
