open CamomileLibraryDefault

let sub s min max =
  let open Camomile in
  let rec aux buf s pos max =
    if pos >= max then () else
    let c = UTF8.get s pos in
    let () = UTF8.Buf.add_char buf c in
    aux buf s (pos + 1) max
  in
  let buf = UTF8.Buf.create (max - min) in
  let () = aux buf s min max in
  UTF8.Buf.contents buf

let length = Camomile.UTF8.length
