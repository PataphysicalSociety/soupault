(* Simple wrappers for hash functions from Digestif
   that just take a string and return a hex digest.
 *)

let blake2s s =
  let ctx = Digestif.BLAKE2S.empty in
  let ctx = Digestif.BLAKE2S.feed_string ctx s in
  Digestif.BLAKE2S.get ctx |> Digestif.BLAKE2S.to_hex



