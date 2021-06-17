(* Errors *)

exception Key_error of string
exception Type_error of string

let key_error err = raise (Key_error err)
let type_error err = raise (Type_error err)

(* Types *)

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
  | TomlOffsetDateTime of string
  | TomlLocalDateTime of string
  | TomlLocalDate of string
  | TomlLocalTime of string
  | TomlArray of t list
  | TomlTable of (string * t) list
  | TomlInlineTable of (string * t) list
  | TomlTableArray of t list

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
  | TomlArray _ | TomlTableArray _ -> Array
  | TomlTable _ | TomlInlineTable _ -> Table

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
    Toml.Types.Table.fold (fun k _ ks -> (Toml.Types.Table.Key.to_string k) :: ks ) table [] |> List.rev

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

let force_inline v =
  match v with
  | TomlTable t -> TomlInlineTable t
  | _ as v -> v

type formatter_settings = {
  indent_width: int;
  indent_character: char;
  indent_subtables: bool;
  newline_before_table: bool
}

let make_indent indent settings level =
  if not indent then "" else
  String.make (settings.indent_width * level) settings.indent_character

let rec format_primitive ?(table_path=[]) ?(inline=false) ?(table_array=false) ?(indent=true) ?(indent_level=0) settings callback v =
  match v with
  | TomlString s ->
    callback "\""; callback @@ Oprint.escape_string s; callback "\""
  | TomlInteger i ->
    callback @@ string_of_int i
  | TomlFloat f ->
    callback @@ Printf.sprintf "%.2f" f
  | TomlBoolean b ->
    callback @@ string_of_bool b
  | TomlOffsetDateTime dt ->
    callback dt
  | TomlLocalDateTime dt ->
    callback dt
  | TomlLocalDate dt ->
    callback dt
  | TomlLocalTime t ->
    callback t
  | TomlArray a ->
    let a = List.map force_inline a in
    let last_index = (List.length a) - 1 in
    callback "[";
    List.iteri (fun n v ->
      (* Nothing inside an array should be indented. *)
      format_primitive ~indent:false settings callback v;
      (* Avoid trailing commas after the last item (even though the 1.0 spec allows them). *)
      if n <> last_index then callback ", ")
    a;
    callback "]"
  | TomlTable t ->
    let () =
      if (table_path <> []) then begin
        if settings.newline_before_table then callback "\n";
        (* Table headers look best when they are at the same indent level as the parent table's keys.
           Since the indent level is incremented by the format_pair function,
           when this function is called on a nested table, the indent level is what it should be
           for the _current table keys_.
           To compensate for this, we decrement the level by one for header printing. *)
        let indent_string = make_indent indent settings (indent_level - 1) in
        if table_array then callback @@ Printf.sprintf "%s[[%s]]\n" indent_string (String.concat "." table_path)
        else callback @@ Printf.sprintf "%s[%s]\n" indent_string (String.concat "." table_path)
      end
    in
    let inline = if table_array then false else inline in
    let t = if table_array then List.map (fun (k, v) -> (k, force_inline v)) t else t in
    let f = format_pair ~table_path:table_path
              ~indent:indent ~indent_level:indent_level ~inline:inline
              ~table_array:table_array
              settings callback
    in
    List.iter f t
  | TomlInlineTable t ->
    let last_index = (List.length t) - 1 in
    callback "{";
    List.iteri (fun n (k, v) ->
      callback @@ Printf.sprintf "%s = " k;
      (* If an _inline_ table contains other tables or table arrays,
         we have to force them all to inline table format to produce valid TOML. *)
      let v = force_inline v in
      (* We also need to disable key indentation, else it will look weird. *)
      format_primitive ~table_path:[] ~indent:false settings callback v;
      if n <> last_index then callback ", ")
    t;
    callback "}"
  | TomlTableArray _ ->
    (* A non-inline table array must have a [[$name]] header, but $name has to come from somewhere,
       so, unlike other values, it's impossible to render it in isolation.
       Only the render_pair function called from a table can render table arrays correctly. *)
    failwith "TOML arrays of tables cannot be formatted out of the parent table context"
and format_pair ?(table_path=[]) ?(indent=true) ?(indent_level=0) ?(inline=false) ?(table_array=false) settings callback (k, v) =
  match v with
  | TomlTable _ as v ->
    let indent_level =
      if settings.indent_subtables then indent_level + 1
      else if indent_level < 1 then indent_level + 1 else indent_level
    in
    format_primitive ~table_path:(table_path @ [k]) ~indent_level:indent_level ~table_array:table_array
      settings callback v
  | TomlTableArray v ->
    let v = List.map (fun v -> (k, v)) v in
    let f = format_pair ~table_path:table_path ~indent:indent ~indent_level:indent_level
              ~inline:inline ~table_array:true
              settings callback
    in
    List.iter f v
  | _ as v ->
    callback @@ Printf.sprintf "%s%s = " (make_indent indent settings indent_level) k;
    format_primitive ~table_path:table_path ~indent:indent settings callback v;
    if not inline then callback "\n"

let to_string ?(indent_width=2) ?(indent_character=' ') ?(indent_subtables=false) ?(newline_before_table=true) v =
  let settings = {
    indent_width = indent_width;
    indent_character = indent_character;
    indent_subtables = indent_subtables;
    newline_before_table = newline_before_table
  }
  in
  let buf = Buffer.create 4096 in
  let () = format_primitive settings (Buffer.add_string buf) v in
  Buffer.contents buf

let to_channel ?(indent_width=2) ?(indent_character=' ') ?(indent_subtables=false) ?(newline_before_table=true) chan v =
  let settings = {
    indent_width = indent_width;
    indent_character = indent_character;
    indent_subtables = indent_subtables;
    newline_before_table = newline_before_table
  }
  in
  format_primitive settings (output_string chan) v

let value_of_toml = Toml_reader.value_of_toml
let value_of_table = Toml_reader.value_of_table

let from_file path =
  try
    let data = Toml.Parser.from_filename path |> Toml.Parser.unsafe in
    Ok (value_of_table data)
  with
  | Sys_error err -> Error (Printf.sprintf "Could not read TOML file: %s" err)
  | Toml.Parser.Error (err, _) -> Error (Printf.sprintf "Could not parse TOML file %s: %s" path err)

(* Conversions between different variants of the same type. *)

let table_to_inline t =
  match t with
  | TomlInlineTable _ as t -> t
  | TomlTable t -> TomlInlineTable t
  | _ as t -> Printf.ksprintf type_error "cannot convert %s to an inline table" (type_string t)

let inline_to_table t =
  match t with
  | TomlInlineTable t -> TomlTable t
  | TomlTable _ as t -> t
  | _ as t -> Printf.ksprintf type_error "cannot convert %s to a table" (type_string t)

(* Constructors *)

let string s = TomlString s
let integer n = TomlInteger n
let float n = TomlFloat n
let boolean b = TomlBoolean b
let offset_datetime dt = TomlOffsetDateTime dt
let local_datetime dt = TomlLocalDateTime dt
let local_date d = TomlLocalDate d
let local_time t = TomlLocalTime t
let array xs = TomlArray xs
let table kvs = TomlTable kvs
let inline_table kvs = TomlInlineTable kvs
let table_array xs =
  let is_table t =
    match t with 
    | TomlTable _ | TomlInlineTable _ -> true
    | _ -> false
  in
  if List.for_all is_table xs then TomlTableArray (List.map inline_to_table xs)
  else Printf.ksprintf type_error "cannot create an array of tables: original array contains a non-table item"

(* Accessors *)
let get_table t =
  match t with
  | TomlTable os | TomlInlineTable os -> os
  | _ -> Printf.ksprintf type_error "value is %s, not a table" (type_string t)

let get_string ?(strict=true) t =
  match t with
  | TomlString s -> s
  | _ ->
    begin
      if strict then Printf.ksprintf type_error "value must be a string, found a %s" (type_string t) else
      match t with
      | TomlInteger i -> string_of_int i
      | TomlFloat f -> string_of_float f
      | TomlBoolean b -> string_of_bool b
      | _ -> Printf.ksprintf type_error "cannot convert %s to string" (type_string t)
    end

let get_integer ?(strict=true) t =
  match t with
  | TomlInteger i -> i
  | _ ->
    begin
      if strict then Printf.ksprintf type_error "value must be an integer, found %s" (type_string t) else
      match t with
      | TomlString s -> int_of_string s
      | TomlBoolean b -> (if b then 1 else 0)
      | _ -> Printf.ksprintf type_error "cannot convert %s to integer" (type_string t)
    end

let get_bool ?(strict=true) t =
  match t with
  | TomlBoolean b -> b
  | _ ->
    begin
      if strict then Printf.ksprintf type_error "value must be an boolean, found a %s" (type_string t) else
      match t with
      | TomlString s -> (s = "")
      | TomlInteger i -> (i = 0)
      | TomlFloat f -> (f = 0.0)
      | TomlArray a | TomlTableArray a -> (a = [])
      | TomlTable o | TomlInlineTable o -> (o = [])
      | _ -> false
    end

let get_array ?(strict=true) t =
  match t with
  | TomlArray a | TomlTableArray a -> a
  | _ as v ->
    if strict then Printf.ksprintf type_error "value must be an array, found %s" (type_string t)
    else [v]

(* High-level interfaces *)

let list_table_keys t =
  let t =
    try get_table t
    with Type_error msg -> Printf.ksprintf type_error "cannot list keys: %s" msg
  in
  List.fold_left (fun acc (x, _) -> x :: acc) [] t |> List.rev

let field k t =
  try
    begin
      let t = get_table t in
      let res = List.assoc_opt k t in
      match res with
      | Some res -> res
      | None -> Printf.ksprintf key_error "field \"%s\" not found" k
    end
  with
  | Key_error msg -> Printf.ksprintf key_error "cannot retrieve field \"%s\": %s" k msg
  | Type_error msg -> Printf.ksprintf type_error "cannot retrieve field \"%s\": %s" k msg

let field_opt k t =
  try Some (field k t)
  with Key_error _ -> None

let find accessor value path =
  let make_dotted_path ps = String.concat "." ps in
  let rec aux accessor path value =
    match path with
    | [] -> accessor value
    | p :: ps ->
      let value = field p value in
      aux accessor ps value
  in
  try
    aux accessor path value
  with
  | Key_error msg ->
    Printf.ksprintf key_error "Failed to retrieve a value at %s: %s" (make_dotted_path path) msg
  | Type_error msg ->
    Printf.ksprintf type_error "TOML type error occured while trying to retrieve a value at %s: %s"
      (make_dotted_path path) msg

let find_opt accessor value path =
  try Some (find accessor value path)
  with Key_error _ -> None

let find_or ~default:default value accessor path =
  find_opt accessor value path |> Option.value ~default:default

let update_field value key new_value =
  let rec update assoc key value =
    match assoc with
    | [] ->
      begin
        match value with
        | None -> []
        | Some v -> [(key, v)]
      end
    | (key', value') :: assoc' ->
      if key = key' then
      begin
        match value with
        | None -> assoc'
        | Some v -> (key, v) :: assoc'
      end
      else (key', value') :: (update assoc' key value)
  in
  match value with
  | TomlTable fs -> TomlTable (update fs key new_value)
  | TomlInlineTable fs -> TomlInlineTable (update fs key new_value)
  | _ -> Printf.ksprintf key_error "cannot update field %s: value is %s, not a table" key (type_string value)

let rec update value ?(use_inline_tables=false) path new_value =
  let make_empty_table use_inline =
    if use_inline then (TomlInlineTable []) else (TomlTable [])
  in
  match path with
  | [] -> failwith "Cannot update a TOML value at an empty path"
  | [p] -> update_field value p new_value
  | p :: ps ->
    let nested_value = field_opt p value |> Option.value ~default:(make_empty_table use_inline_tables) in
    let nested_value = update nested_value ps new_value in
    update_field value p (Some nested_value)

(* Convertors *)

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
  | TomlArray xs | TomlTableArray xs -> `A (List.map to_json xs)
  | TomlTable os | TomlInlineTable os -> `O (List.map (fun (k, v) -> (k, to_json v)) os)

  
