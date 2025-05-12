(* This module provides convenience wrappers for Jingoo template engine functions.
   (see http://tategakibunko.github.io/jingoo/)

   Since Jingoo uses its own type system, we need wrappers for converting
   JSON and TOML data to Jingoo's types.
 *)

open Common
open Jingoo

let rec jingoo_of_json j =
  match j with
  | `O o -> List.map (fun (k, v) -> k, jingoo_of_json v) o |> Jg_types.box_obj
  | `A a -> List.map jingoo_of_json a |> Jg_types.box_list
  | `String s -> Jg_types.box_string s
  | `Bool b -> Jg_types.box_bool b
  | `Float f -> Jg_types.box_float f
  | `Null -> Jg_types.Tnull
  | _ ->
    (* Shouldn't happen under normal circumstances,
       that list of JSON types is supposed to be exhaustive. *)
    internal_error "Unimplemented JSON to Jingoo type conversion"

let rec jingoo_of_toml t =
  let open Otoml in
  match t with
  | TomlArray vs -> List.map jingoo_of_toml vs |> Jg_types.box_list
  | TomlTable kvs | TomlInlineTable kvs ->
    List.map (fun (k, v) -> k, jingoo_of_toml v) kvs |> Jg_types.box_obj
  | TomlString s -> Jg_types.box_string s
  | TomlBoolean b -> Jg_types.box_bool b
  | TomlInteger i -> Jg_types.box_int i
  | TomlFloat f -> Jg_types.box_float f
  | TomlOffsetDateTime d | TomlLocalDateTime d | TomlLocalDate d | TomlLocalTime d ->
    (* Exploits the fact that the default OTOML implementation represents
       all datetime values as strings and soupault only uses the default implementation.
     *)
    Jg_types.box_string d
  | _ ->
    (* Shouldn't happen under normal circumstances,
      that list of TOML types is supposed to be exhaustive. *)
    internal_error "Unimplemented TOML to Jingoo type conversion"

let render tmpl data = Jg_template.Loaded.eval ~models:data tmpl

let of_string s =
  try Jg_template.Loaded.from_string ~env:({Jg_types.std_env with autoescape=false}) s
  with _ ->
    (* As of Jingoo 1.4.4, template parse errors raise a generic [Jingoo.Jg_parser.MenhirBasics.Error]
       without any error information attached to it,
       so we have to use a catch-all approach here for now. *)
    let () = Logs.err @@ fun m -> m "Invalid template string:\n%s" s in
    raise (Soupault_error "Failed to parse template. Consult Jingoo documentation for a syntax reference.")
