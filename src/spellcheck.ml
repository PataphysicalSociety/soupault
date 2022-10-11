
(* "Spell checking" for config options based on "normalized" identifiers.

    In code/configs, letter typos (e.g. "Strign.lowrrcase_asci") is just one class of errors.
    Misplaced or wrong delimiters are just as common, and so in incorrect capitalization.

    To get a reasonable chance to guess what the user meant, we strip identifiers
    down to their "essence" by removing all delimieters and using case-insensitive
    string comparison.
 *)

(* As of Spelll 0.3, distance greater than 2 is intolerably slow *)
let max_distance = 2

let remove_delims s =
  let re = Re.Perl.compile_pat {|[_-\./\\]|} in
  Re.replace ~all:true ~f:(fun _ -> "") re s

let normalize s = s |> remove_delims |> String.lowercase_ascii

let make_index words =
  let dict = List.map (fun s -> (normalize s, s)) words in
  Spelll.Index.of_list dict

let get_suggestion index word =
  let word = normalize word in
  let suggestions = Spelll.Index.retrieve_l ~limit:max_distance index word in
  match suggestions with
  | [] -> None
  | s :: _ -> Some s
