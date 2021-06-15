(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Projet Cristal, INRIA Rocquencourt                   *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let escape_string s =
  (* Escape only C0 control characters (bytes <= 0x1F), DEL(0x7F), '\\'
     and '"' *)
   let n = ref 0 in
  for i = 0 to String.length s - 1 do
    n := !n +
      (match String.unsafe_get s i with
       | '\"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
       | '\x00' .. '\x1F'
       | '\x7F' -> 4
       | _ -> 1)
  done;
  if !n = String.length s then s else begin
    let s' = Bytes.create !n in
    n := 0;
    for i = 0 to String.length s - 1 do
      begin match String.unsafe_get s i with
      | ('\"' | '\\') as c ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n c
      | '\n' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'n'
      | '\t' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 't'
      | '\r' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'r'
      | '\b' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'b'
      | '\x00' .. '\x1F' | '\x7F' as c ->
          let a = Char.code c in
          Bytes.unsafe_set s' !n '\\';
          incr n;
          Bytes.unsafe_set s' !n (Char.chr (48 + a / 100));
          incr n;
          Bytes.unsafe_set s' !n (Char.chr (48 + (a / 10) mod 10));
          incr n;
          Bytes.unsafe_set s' !n (Char.chr (48 + a mod 10));
      | c -> Bytes.unsafe_set s' !n c
      end;
      incr n
    done;
    Bytes.to_string s'
  end
