(* Simple wrappers for hash functions from Digestif
   that just take a string and return a hex digest.
 *)

let blake2s s =
  let ctx = Digestif.BLAKE2S.empty in
  let ctx = Digestif.BLAKE2S.feed_string ctx s in
  Digestif.BLAKE2S.get ctx |> Digestif.BLAKE2S.to_hex

let blake2b s =
  let ctx = Digestif.BLAKE2B.empty in
  let ctx = Digestif.BLAKE2B.feed_string ctx s in
  Digestif.BLAKE2B.get ctx |> Digestif.BLAKE2B.to_hex

let md5 s =
  let ctx = Digestif.MD5.empty in
  let ctx = Digestif.MD5.feed_string ctx s in
  Digestif.MD5.get ctx |> Digestif.MD5.to_hex

let sha1 s =
  let ctx = Digestif.SHA1.empty in
  let ctx = Digestif.SHA1.feed_string ctx s in
  Digestif.SHA1.get ctx |> Digestif.SHA1.to_hex

let sha256 s =
  let ctx = Digestif.SHA256.empty in
  let ctx = Digestif.SHA256.feed_string ctx s in
  Digestif.SHA256.get ctx |> Digestif.SHA256.to_hex

let sha512 s =
  let ctx = Digestif.SHA512.empty in
  let ctx = Digestif.SHA512.feed_string ctx s in
  Digestif.SHA512.get ctx |> Digestif.SHA512.to_hex
