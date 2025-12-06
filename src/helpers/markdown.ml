
(* "Smart punctuation" *)

let substitutions = [
  (* Quotes and similar characters *)

  (* Double quotes facing in correct directions *)
  (Re.Perl.compile_pat "``", "&ldquo;");
  (Re.Perl.compile_pat "''", "&rdquo;");

  (* Apostrophe — must be after the right double quote regex
     to avoid converting "''" into "&apos;&apos;".
     We use &rdquo; for a proper typographic apostrophe here,
     &apos; looks like a "typewriter quote" in most cases.
   *)
  (Re.Perl.compile_pat "'", "&rsquo;");

  (* Dashes *)

  (* The long em dash *)
  (Re.Perl.compile_pat "---", "&mdash;");

  (* The shorter en dash, must be after the em dash regex
     to avoid converting "---" into "&ndash;-"
   *)
  (Re.Perl.compile_pat "--", "&ndash;");

  (* Ellipsis *)
  (Re.Perl.compile_pat {|\.\.\.|}, "&hellip;");

]

let substitute substitutions str =
  let rec aux subs str =
    match subs with
    | [] -> str	
    | (re, sub) :: rest ->
      let new_str = Re.replace ~all:true ~f:(fun _ -> sub) re str in
      aux rest new_str
  in aux substitutions str

type Cmarkit.Block.t += Doc of Cmarkit.Doc.t

let smart_punctuation_html =
  let inline ctx node =
    match node with
    | Cmarkit.Inline.Text (t, _) ->
      let res = substitute substitutions t in
      let () = Cmarkit_renderer.Context.string ctx res in
      true
    | _	->
      (* Let the default rendered handle everything else *)
      false
  in
  let block ctx node =
    match node with
    | Doc d ->
      (* It's important to recurse via Cmarkit_renderer.Context.block *)
      Cmarkit_renderer.Context.block ctx (Cmarkit.Doc.block d); true
    | _ -> 
      (* Let the default HTML renderer handle everything else *)
      false
  in
  Cmarkit_renderer.make ~inline ~block ()

let smart_punctuation_html_of_doc ~safe doc =
  let default = Cmarkit_html.renderer ~safe () in
  let r = Cmarkit_renderer.compose default smart_punctuation_html in
  Cmarkit_renderer.doc_to_string r doc
