
type toml_type =
  | String
  | Integer
  | Float
  | Boolean
  | LocalTime
  | LocalDate
  | LocalDateTime
  | OffsetDateTime
  | Array
  | Table

type t =
  | TomlString of string
  | TomlInteger of int
  | TomlFloat of float
  | TomlBoolean of bool
  | TomlLocalTime of string
  | TomlLocalDate of string
  | TomlLocalDateTime of string
  | TomlOffsetDateTime of string
  | TomlArray of t list
  | TomlTable of (string * t) list
  | TomlInlineTable of (string * t) list 
  | TomlTableArray of (string * t) list

let type_of_value v =
  match v with
  | TomlString _ -> String
  | TomlInteger _ -> Integer
  | TomlFloat _ -> Float
  | TomlBoolean _ -> Boolean
  | TomlLocalTime _ -> LocalTime
  | TomlLocalDate _ -> LocalDate
  | TomlLocalDateTime _ -> LocalDateTime
  | TomlOffsetDateTime _ -> OffsetDateTime
  | TomlArray _ -> Array
  | TomlTable _ | TomlInlineTable _ | TomlTableArray _ -> Table

let string_of_type t =
  match t with
  | String -> "string"
  | Integer -> "integer"
  | Float -> "float"
  | Boolean -> "boolean"
  | LocalTime -> "local time"
  | LocalDate -> "local date"
  | LocalDateTime -> "local date-time"
  | OffsetDateTime -> "offset date-time"
  | Array -> "array"
  | Table -> "table"

let type_string v = type_of_value v |> string_of_type

module Toml_reader = struct
  let (>>=) = Option.bind

 (* List all keys of a TOML table
    This is used to retrieve a list of widgets to call
  *)
  let list_table_keys table =
    Toml.Types.Table.fold (fun k _ ks -> (Toml.Types.Table.Key.to_string k) :: ks ) table []

  let get_field k tbl = Toml.Lenses.(get tbl (key k))

  let get_field_unsafe k tbl =
    let v = get_field k tbl in
    match v with
    | Some v -> v
    | None -> failwith "get_field_unsafe was called in a place where it really was unsafe"

  let get_string t = Toml.Lenses.string.get t >>= (fun x -> Some (TomlString x))
  let get_bool t = Toml.Lenses.bool.get t >>= (fun x -> Some (TomlBoolean x))
  let get_int t = Toml.Lenses.int.get t >>= (fun x -> Some (TomlInteger x))
  let get_float t = Toml.Lenses.float.get t >>= (fun x -> Some (TomlFloat x))

  (* There is no generic list retrieval lens, and to.ml doesn't support heterogenous arrays
     (even though the TOML spec does).
     So, until we have a better library, that's what we've got
   *)
  let get_strings t = Toml.Lenses.(array.get t >>= strings.get) >>= (fun xs -> Some (TomlArray (List.map (fun x -> TomlString x) xs)))
  let get_bools t = Toml.Lenses.(array.get t >>= bools.get) >>= (fun xs -> Some (TomlArray (List.map (fun x -> TomlBoolean x) xs)))
  let get_ints t = Toml.Lenses.(array.get t >>= ints.get) >>= (fun xs -> Some (TomlArray (List.map (fun x -> TomlInteger x) xs)))
  let get_floats t = Toml.Lenses.(array.get t >>= floats.get) >>= (fun xs -> Some (TomlArray (List.map (fun x -> TomlFloat x) xs)))

  let rec value_of_toml toml =
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
  and value_of_table t =
    let keys = list_table_keys t in
    let values = List.map (fun k -> k, (get_field_unsafe k t |> value_of_toml)) keys in
    (TomlTable values)
  and value_getters = [
    get_string; get_bool; get_int; get_float;
    get_strings; get_bools; get_ints; get_floats;
    (fun t -> Toml.Lenses.table.get t >>= (fun x -> Some (value_of_table x)))
  ]
end

let value_of_toml = Toml_reader.value_of_toml
let value_of_table = Toml_reader.value_of_table

let from_file path =
  try
    let data = Toml.Parser.from_filename path |> Toml.Parser.unsafe in
    Ok (value_of_table data)
  with
  | Sys_error err -> Error (Printf.sprintf "Could not read TOML file: %s" err)
  | Toml.Parser.Error (err, _) -> Error (Printf.sprintf "Could not parse TOML file %s: %s" path err)

exception Key_error of string
exception Type_error of string

let key_error err = raise (Key_error err)
let type_error err = raise (Type_error err)

let list_table_keys t =
  match t with
  | TomlTable os | TomlInlineTable os | TomlTableArray os -> Utils.assoc_keys os
  | _ -> Printf.ksprintf type_error "value must be a table, found %s" (type_string t)

let field ?(default=None) ?(getter=(fun x -> x)) k j =
  match j with
  | TomlTable fs | TomlInlineTable fs | TomlTableArray fs -> begin
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

let get ?(conv=(fun x -> x)) typ path value =
  let rec aux path value =
    match (path, value) with
    | [], v ->
      (* Apply type conversion, if a user supplied one *)
      let v = conv v in
      let value_type = type_of_value v in
      if value_type = typ then Ok v
      else Error (Printf.sprintf
        "Expected a value of type %s but the actual type is %s"
        (type_string v)
        (string_of_type typ))
    | p :: ps, (TomlTable t) ->
      let res = List.assoc_opt p t in
      (match res with
       | Some v -> aux ps v
       | None -> Error (Printf.sprintf "table does not have key \"%s\"" p))
    | _, _ -> Error ""
  in aux path value

let string ?(strict=true) j =
  match j with
  | TomlString s -> s
  | _ -> begin
    if strict then Printf.ksprintf type_error "value must be a string, found a %s" (type_string j) else
    match j with
    | TomlInteger i -> string_of_int i
    | TomlFloat f -> Utils.string_of_float f
    | TomlBoolean b -> string_of_bool b
    | _ -> Printf.ksprintf type_error "cannot convert %s to string" (type_string j)
  end

let integer ?(strict=true) j =
  match j with
  | TomlInteger i -> i
  | _ -> begin
    if strict then Printf.ksprintf type_error "value must be an integer, found %s" (type_string j) else
    match j with
     | TomlString s -> int_of_string s
     | TomlBoolean b -> (if b then 1 else 0)
     | _ -> Printf.ksprintf type_error "cannot convert %s to integer" (type_string j)
 end

let bool ?(strict=true) j =
  match j with
  | TomlBoolean b -> b
  | _ -> begin
    if strict then Printf.ksprintf type_error "value must be an boolean, found a %s" (type_string j) else
    match j with
    | TomlString s -> (s = "")
    | TomlInteger i -> (i = 0)
    | TomlFloat f -> (f = 0.0)
    | TomlArray a -> (a = [])
    | TomlTable o | TomlInlineTable o | TomlTableArray o ->  (o = [])
    | _ -> false
  end

let table j =
  match j with
  | (TomlTable _ | TomlInlineTable _ | TomlTableArray _) as o -> o
  | _ -> type_error "value must be a table"

let strings ?(strict=true) js = List.map (fun j -> string ~strict:strict j) js

let list ?(strict=true) j =
  match j with
  | TomlArray a -> a
  | _ as v ->
    if strict then type_error "value must be a list"
    else [v]

let rec of_json j =
  match j with
  | `Float n -> TomlFloat n
  | `Bool b -> TomlBoolean b
  | `String s -> TomlString s
  | `A js -> TomlArray (List.map of_json js)
  | `O os -> TomlTable (List.map (fun (k, v) -> (k, of_json v)) os)
  | `Null -> TomlTable []

let rec to_json t =
  match t with
  | TomlString s -> `String s
  | TomlInteger i -> `Float (float_of_int i)
  | TomlFloat f -> `Float f
  | TomlBoolean b -> `Bool b
  | TomlLocalTime s -> `String s
  | TomlLocalDate s -> `String s
  | TomlLocalDateTime s -> `String s
  | TomlOffsetDateTime s -> `String s
  | TomlArray xs -> `A (List.map to_json xs)
  | TomlTable os | TomlInlineTable os | TomlTableArray os -> `O (List.map (fun (k, v) -> (k, to_json v)) os)

  
