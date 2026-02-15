(* Cmarkit (as of 0.0.4) does not have native support for "smart punctuation",
   i.e., replacement of ASCII punctuation characters and sequences
   with nicer-looking versions, like "---" with "&mdash;"/"—"
   or ``something'' with "&ldquo;something&rdquo;".

   It does, however, provide functionality for composing custom renderers,
   and here we use it to allow the user to choose what substitutions they want.
 *)

open Common

module CCtx = Cmarkit_renderer.Context

(* "Smart punctuation" substitution maps *)

(* Quotes and similar characters *)

(* Double quotes facing in correct directions *)
let smart_quotes = [
  ("``", "&ldquo;");
  ("''", "&rdquo;");
]

(* We use "&rsquo;" for a proper typographic apostrophe here
   because "&apos;" looks like a "typewriter quote" in most fonts.
 *)
let smart_apostrophe = [
  ("'", "&rsquo;");
]

(* Dashes *)
let smart_dashes = [
  (* The long em dash *)
  ("---", "&mdash;");

  (* The shorter en dash, must be after the em dash regex
     to avoid converting "---" into "&ndash;-"
   *)
  ("--", "&ndash;");
]

(* Ellipsis *)
let smart_ellipsis = [
  ({|\.\.\.|}, "&hellip;");
]

(* Substitution logic *)

let compile_substitutions ss =
  List.map (fun (re, sub) -> (Re.Perl.compile_pat re, sub)) ss

let substitute substitutions str =
  let rec aux subs str =
    match subs with
    | [] -> str	
    | (re, sub) :: rest ->
      let new_str = Re.replace ~all:true ~f:(fun _ -> sub) re str in
      aux rest new_str
  in aux substitutions str

let make_substitution_map settings =
  (* Use an empty map if smart punctuation is disabled in the config *)
  if not settings.markdown_smart_punctuation then [] else
  (* If it's enabled, compose a map according to the settings.
     By default, everything is enabled but the user can disable individual options *)
  let map = [] in
  let map = if settings.markdown_smart_quotes then map @ smart_quotes else map in
  (* Add the smart apostrophe reges only after quotes,
     to avoid replacing "''" with "&rsquo;&rsquo;"
   *)
  let map = if settings.markdown_smart_apostrophe then map @ smart_apostrophe else map in
  let map = if settings.markdown_smart_dashes then map @ smart_dashes else map in
  let map = if settings.markdown_smart_ellipsis then map @ smart_ellipsis else map in
  map

(* Markdown renderering function maker *)

type Cmarkit.Block.t += Doc of Cmarkit.Doc.t

let smart_punctuation_renderer settings =
  (* Prepare the substitution map *)
  let substitution_map = make_substitution_map settings in
  (* Pre-compile regexes to avoid wasting CPU time on that
     every time the Markdown rendering function is called
   *)
  let substitutions = compile_substitutions substitution_map in
  let inline ctx node =
    match node with
    | Cmarkit.Inline.Text (t, _) ->
      (* Handle text nodes.
         Text inside code spans and blocks is not under [Inline.Text],
         so we don't need to do anything special to avoid mangling it.
       *)
      let res = substitute substitutions t in
      let () = Cmarkit_renderer.Context.string ctx res in
      true
    | _	->
      (* Let the default rendered handle everything else *)
      false
  in
  Cmarkit_renderer.make ~inline ()

let doc_block_renderer () =
  let block ctx node =
    match node with
    | Doc d ->
      (* It's important to recurse via Cmarkit_renderer.Context.block *)
      Cmarkit_renderer.Context.block ctx (Cmarkit.Doc.block d); true
    | _ -> 
      (* Let the default HTML renderer handle everything else *)
      false
  in
  Cmarkit_renderer.make ~block ()

let math_renderer () =
  let math_span c ms =
    let tex_line c l = Cmarkit_html.html_escaped_string c (Cmarkit.Block_line.tight_to_string l) in
    let tex_lines c = function (* newlines only between lines *)
    | [] -> () | l :: ls ->
        let line c l = CCtx.byte c '\n'; tex_line c l in
        tex_line c l; List.iter (line c) ls
    in
    let tex = Cmarkit.Inline.Math_span.tex_layout ms in
    if tex = [] then () else
    (CCtx.string c (if Cmarkit.Inline.Math_span.display ms then "<span class=\"math-display\">" else "<span class=\"math-inline\">");
    tex_lines c tex;
    CCtx.string c "</span>")
  in
  let inline ctx node =
    match node with
    | Cmarkit.Inline.Ext_math_span (ms, _) -> math_span ctx ms; true
    | _ -> 
      (* Let the default HTML renderer handle everything else *)
      false
  in
  let math_block ctx node =
    let line l = Cmarkit_html.html_escaped_string ctx (Cmarkit.Block_line.to_string l); CCtx.byte ctx '\n' in
    CCtx.string ctx "<div class=\"math-display\">";
    List.iter line (Cmarkit.Block.Code_block.code node);
    CCtx.string ctx "</div>"
  in
  let block ctx node =
    match node with
    | Cmarkit.Block.Ext_math_block (node, _) -> math_block ctx node; true
    | _ -> 
      (* Let the default HTML renderer handle everything else *)
      false
  in
  Cmarkit_renderer.make ~inline ~block ()

let make_markdown_renderer settings =
  let punct = smart_punctuation_renderer settings in
  let docb = doc_block_renderer () in
  let math = math_renderer () in
  let html_renderer ~safe doc =
    let default = Cmarkit_html.renderer ~safe () in
    let r =
      default
      |> fun d -> Cmarkit_renderer.compose d punct
      |> fun d -> Cmarkit_renderer.compose d docb
      |> fun d ->
        if settings.markdown_math_delimiters_html then
          Cmarkit_renderer.compose d math
        else
          d
    in
    Cmarkit_renderer.doc_to_string r doc
  in
  let render_markdown source =
    Cmarkit.Doc.of_string ~strict:settings.markdown_strict_commonmark source |>
    html_renderer ~safe:false
  in
  render_markdown

(* The Markdown renderer instance.
   For simplicity, we create the default instance at compile time here,
   which will be updated at runtime with settings from the config.
 *)
let render_markdown = ref @@ make_markdown_renderer Common.default_settings
