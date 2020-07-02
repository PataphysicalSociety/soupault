(* List all keys of a TOML table
   This is used to retrieve a list of widgets to call
 *)
let list_table_keys table =
  TomlTypes.Table.fold (fun k _ ks -> (TomlTypes.Table.Key.to_string k) :: ks ) table []

type value = [
    `String of string
  | `Int of int
  | `Float of float
  | `Bool of bool
  | `A of value list
  | `O of (string * value) list
]

let (>>=) = Option.bind

let get_string k tbl = TomlLenses.(get tbl (key k |-- string)) >>= (fun x -> Some (`String x))
let get_bool k tbl = TomlLenses.(get tbl (key k |-- bool)) >>= (fun x -> Some (`Bool x))
let get_int k tbl = TomlLenses.(get tbl (key k |-- int)) >>= (fun x -> Some (`Int x))
let get_float k tbl = TomlLenses.(get tbl (key k |-- float)) >>= (fun x -> Some (`Float x))

let get_strings k tbl = TomlLenses.(get tbl (key k |-- array |-- strings)) >>= (fun xs -> Some (`A (List.map (fun x -> `String x) xs)))
let get_bools k tbl = TomlLenses.(get tbl (key k |-- array |-- bools)) >>= (fun xs -> Some (`A (List.map (fun x -> `Bool x) xs)))
let get_ints k tbl = TomlLenses.(get tbl (key k |-- array |-- ints)) >>= (fun xs -> Some (`A (List.map (fun x -> `Int x) xs)))
let get_floats k tbl = TomlLenses.(get tbl (key k |-- array |-- floats)) >>= (fun xs -> Some (`A (List.map (fun x -> `Float x) xs)))

let rec get_value k tbl =
  let rec aux k tbl fs =
    match fs with
    | [] -> `Null
    | f :: fs ->
      let v = f k tbl in begin
        match v with
        | None -> aux k tbl fs
        | Some v -> v
      end
  in aux k tbl getters
and get_table k tbl =
  let table = TomlLenses.(get tbl (key k |-- table)) in
  match	table with 
  | None -> None
  | Some t ->
    let	keys = list_table_keys t in
    let	values = List.map (fun k -> k, (get_value k t)) keys in
    Some (`O values)

and getters = [get_string; get_bool; get_int; get_float; get_strings; get_bools; get_ints; get_floats; get_table]

let assoc_of_table t = `O (list_table_keys t |> List.map (fun k -> k, (get_value k t)))
