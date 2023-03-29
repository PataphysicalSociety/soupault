(** This module mostly provides UTF8 string functions
    that support fallback to ASCII for strings that aren't valid UTF-8.

    In a sense, it's a home-grown alternative to https://opam.ocaml.org/packages/text/
    That package works well, the only problem with it for soupault is that it depends on libiconv
    and doesn't provide a static linking option.

    One of soupault's goals is to provide official self-contained, "eternal" binaries,
    and any dependency on a dynamically linked library would make that impossible.

    This module is based on Camomile (https://opam.ocaml.org/packages/camomile/),
    which is pure OCaml, so static linking with it is effortless.
    However, Camomile's API is somewhat unwieldy, so this module provides
    convenient wrappers and automatic fallback to ASCII functions
    for strings that contain invalid UTF-8 sequences.

    It also containes a few more string handling functions —
    they had to go somewhere, you know. ;)
 *) 

(* Checks if a string is valid UTF-8 or not.
   Other functions rely on it to provide an ASCII fallback
   for strings that cannot be treated as UTF-8 due to invalid characters in them. *)
let is_valid s =
  try
    let () = Camomile.UTF8.validate s in
    true
  with Camomile.UTF8.Malformed_code ->
    false

(** Extracts substrings.
   If a string is valid UTF-8, extracts a range of Unicode characters.
   If not, extracts a range of bytes.
 *)
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

(** Calculates string lengths.
    For valid UTF-8 strings that's length in Unicode characters.
    In other cases it's length in bytes.
 *)
let length s =
  if not (is_valid s) then String.length s else
  Camomile.UTF8.length s

(** Percent-encodes strings for URLs.
   [exclude_characters] allows for "soft" encoding if the user supplies a list of characters to exclude from encoding.
   By default, encodes all characters outside the unreserved set. *)
let url_encode ?(exclude_chars=None) s =
  let is_reserved c =
    match c with
    | 'a' .. 'z' | 'A' .. 'Z' | '-' | '_' | '.' | '~' -> false
    | _ -> true 
  in
  let needs_encoding exclude_chars c =
    match exclude_chars with
    | None -> is_reserved c
    | Some cs ->
      (is_reserved c) && (not @@ List.exists ((=) c) cs)
  in
  let add_char buf c =
    if needs_encoding exclude_chars c then Buffer.add_string buf @@ Printf.sprintf "%%%02X" (Char.code c)
    else Buffer.add_char buf c
  in
  (* Allocate memory for the worst case when every character needs to be encoded.
     Since most URLs are short strings, this is arguably better than re-allocations. *)
  let buf = Buffer.create @@ (String.length s) * 3 in
  let () = String.iter (add_char buf) s in
  Buffer.contents buf

(** Decodes percent-encoded URLs.
    Copied from ocaml-uri (https://github.com/mirage/ocaml-uri),
    distributed under the MIT license.
 *)
let url_decode b =
  let int_of_hex_char c =
    let c = int_of_char (Char.uppercase_ascii c) - 48 in
    if c > 9 then
      if c > 16 && c < 23 then c - 7
      else failwith "int_of_hex_char"
    else if c >= 0 then c
    else failwith "int_of_hex_char"
    in
    let len = String.length b in
    let buf = Buffer.create len in
    let rec scan start cur =
      if cur >= len then Buffer.add_substring buf b start (cur-start)
      else if b.[cur] = '%' then begin
        Buffer.add_substring buf b start (cur-start);
        let cur = cur + 1 in
        if cur >= len then Buffer.add_char buf '%'
        else match int_of_hex_char b.[cur] with
        | exception _ ->
          Buffer.add_char buf '%';
          scan cur cur
        | highbits -> begin
          let cur = cur + 1 in
          if cur >= len then begin
            Buffer.add_char buf '%';
            Buffer.add_char buf b.[cur-1]
          end else begin
            let start_at =
              match int_of_hex_char b.[cur] with
              | lowbits ->
                Buffer.add_char buf (Char.chr (highbits lsl 4 + lowbits));
                cur+1
              | exception _ ->
                Buffer.add_char buf '%';
                Buffer.add_char buf b.[cur-1];
                cur
            in scan start_at start_at
          end
        end
      end else scan start (cur+1)
    in
    scan 0 0;
    Buffer.contents buf
