type value = [
    `String of string
  | `Int of int
  | `Float of float
  | `Bool of bool
  | `A of value list
  | `O of (string * value) list
  | `Null
]

module Toml_reader = struct
  let (>>=) = Option.bind

 (* List all keys of a TOML table
    This is used to retrieve a list of widgets to call
  *)
  let list_table_keys table =
    TomlTypes.Table.fold (fun k _ ks -> (TomlTypes.Table.Key.to_string k) :: ks ) table []

  let get_field k tbl = TomlLenses.(get tbl (key k))

  let get_field_unsafe k tbl =
    let v = get_field k tbl in
    match v with
    | Some v -> v
    | None -> failwith "get_field_unsafe was called in a place where it was really unsafe"

  let get_string t = TomlLenses.string.get t >>= (fun x -> Some (`String x))
  let get_bool t = TomlLenses.bool.get t >>= (fun x -> Some (`Bool x))
  let get_int t = TomlLenses.int.get t >>= (fun x -> Some (`Int x))
  let get_float t = TomlLenses.float.get t >>= (fun x -> Some (`Float x))

  (* There is no generic list retrieval lens, and to.ml doesn't support heterogenous arrays
     (even though the TOML spec does).
     So, until we have a better library, that's what we've got
   *)
  let get_strings t = TomlLenses.(array.get t >>= strings.get) >>= (fun xs -> Some (`A (List.map (fun x -> `String x) xs)))
  let get_bools t = TomlLenses.(array.get t >>= bools.get) >>= (fun xs -> Some (`A (List.map (fun x -> `Bool x) xs)))
  let get_ints t = TomlLenses.(array.get t >>= ints.get) >>= (fun xs -> Some (`A (List.map (fun x -> `Int x) xs)))
  let get_floats t = TomlLenses.(array.get t >>= floats.get) >>= (fun xs -> Some (`A (List.map (fun x -> `Float x) xs)))

  let rec json_of_toml toml =
    let rec aux toml getters =
      match getters with
      | [] ->
        (* There's also date type, which soupault doesn't use *)
        failwith "Unsupported TOML type"
      | g :: gs' ->
        let v = g toml in begin
          match v with
          | None -> aux toml gs'
          | Some v -> v
        end
    in aux toml value_getters
  and json_of_table t =
    let keys = list_table_keys t in
    let values = List.map (fun k -> k, (get_field_unsafe k t |> json_of_toml)) keys in
    (`O values)
  and value_getters = [
    get_string; get_bool; get_int; get_float;
    get_strings; get_bools; get_ints; get_floats;
    (fun t -> TomlLenses.table.get t >>= (fun x -> Some (json_of_table x)))
  ]
end

let json_of_toml = Toml_reader.json_of_toml
let json_of_table = Toml_reader.json_of_table

exception Key_error of string
exception Type_error of string

let key_error err = raise (Key_error err)
let type_error err = raise (Type_error err)

let list_table_keys t =
  match t with
  | `O os -> Utils.assoc_keys os
  | _ -> type_error "value must be a table"

let field ?(default=None) ?(getter=(fun x -> x)) k j =
  match j with
  | `O fs -> begin
      try
        let res = List.assoc k fs in
        getter res
      with
      | Not_found ->
        (match default with
         | None -> Printf.ksprintf key_error "table has no field \"%s\"" k
         | Some default -> default)
      | Type_error e ->
        Printf.ksprintf type_error "wrong value type for the field \"%s\": %s" k e
    end
  | _ -> Printf.ksprintf type_error "cannot retrieve field \"%s\": value is not a table" k

let string ?(strict=true) j =
  match j with
  | `String s -> s
  | _ -> begin
    if strict then type_error "value must be a string" else
    match j with
    | `Int i -> string_of_int i
    | `Float f -> string_of_float f
    | `Bool b -> string_of_bool b
    | `Null -> ""
    | _ -> type_error "cannot to convert an array or table to string"
  end

let int ?(strict=true) j =
  match j with
  | `Int i -> i
  | _ -> begin
    if strict then type_error "value must be an integer" else
    match j with
     | `String s -> float_of_string s |> int_of_float
     | `Float f -> Float.round f |> int_of_float
     | `Bool b -> (if b then 1 else 0)
     | `Null -> 0
     | _ -> type_error "cannot to convert an array or table to integer"
 end

let table j =
  match j with
  | `O _ as o -> o
  | _ -> type_error "value must be a table"

let strings ?(strict=true) js = List.map (fun j -> string ~strict:strict j) js

let list ?(strict=true) j =
  match j with
  | `A a -> a
  | _ as v ->
    if strict then type_error "value must be a list"
    else [v]
