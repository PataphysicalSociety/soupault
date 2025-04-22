(* This should better be a Map *)
let widgets = [
  (* File and output inclusion *)
  ("include", Inclusion_widgets.include_file);
  ("insert_html", Inclusion_widgets.insert_html);
  ("exec", Inclusion_widgets.include_program_output);
  ("preprocess_element", Inclusion_widgets.preprocess_element);

  (* HTML manipulation *)
  ("element_template", Html_widgets.element_template);
  ("delete_element", Html_widgets.delete_element);
  ("wrap", Html_widgets.wrap);

  (* Content extraction and transformation *)
  ("title", Title_widget.set_title);
  ("breadcrumbs", Breadcrumbs_widget.breadcrumbs);
  ("footnotes", Footnotes_widget.footnotes);
  ("toc", Toc_widget.toc);

  (* Link manipulation *)
  ("relative_links", Link_widgets.relative_links);
  ("absolute_links", Link_widgets.absolute_links);
]
