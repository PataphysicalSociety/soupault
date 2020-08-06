open Jingoo

type t = Jg_template.Loaded.t

let rec jingoo_of_json j =
  match j with
  | `O o -> List.map (fun (k, v) -> k, jingoo_of_json v) o |> Jg_types.box_obj
  | `A a -> List.map jingoo_of_json a |> Jg_types.box_list
  | `String s -> Jg_types.box_string s
  | `Bool b -> Jg_types.box_bool b
  | `Float f -> Jg_types.box_float f
  | `Null -> Jg_types.Tnull
  | _ -> failwith "Unimplemented JSON to Jingoo type conversion"

let render tmpl data = Jg_template.Loaded.eval ~models:data tmpl

let of_string s = Jg_template.Loaded.from_string ~env:({Jg_types.std_env with autoescape=false}) s
