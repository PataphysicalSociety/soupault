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
