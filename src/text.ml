open CamomileLibraryDefault

let is_valid s =
  try
    let () = Camomile.UTF8.validate s in
    true
  with Camomile.UTF8.Malformed_code ->
    false

let sub s min max =
  let open Camomile in
  let rec aux buf s pos max =
    if pos >= max then () else
    let c = UTF8.get s pos in
    let () = UTF8.Buf.add_char buf c in
    aux buf s (pos + 1) max
  in
  if not (is_valid s) then String.sub s min max else
  let buf = UTF8.Buf.create (max - min) in
  let () = aux buf s min max in
  UTF8.Buf.contents buf

let length s =
  if not (is_valid s) then String.length s else
  Camomile.UTF8.length s

(* Allow for "soft" encoding if the user supplies a list of character that needs encoding,
   but default to encoding all characters outside the unreserved set. *)
let url_encode ?(chars=None) s =
  let is_reserved c =
    match c with
    | 'a' .. 'z' | 'A' .. 'Z' | '-' | '_' | '.' | '~' -> false
    | _ -> true 
  in
  let needs_encoding chars c =
    match chars with
    | None -> is_reserved c
    | Some cs ->
      List.exists ((=) c) cs
  in
  let add_char buf c =
    if needs_encoding chars c then Buffer.add_string buf @@ Printf.sprintf "%%%02X" (Char.code c)
    else Buffer.add_char buf c
  in
  (* Allocate memory for the worst case when every character needs to be encoded.
     Since most URLs are short strings, this is arguably better than re-allocations. *)
  let buf = Buffer.create @@ (String.length s) * 3 in
  let () = String.iter (add_char buf) s in
  Buffer.contents buf
