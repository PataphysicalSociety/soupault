(* This should better be a Map *)
let widgets = [
  ("include", Inclusion_widgets.include_file);
  ("insert_html", Inclusion_widgets.insert_html);
  ("delete_element", Html_widgets.delete_element);
  ("exec", Inclusion_widgets.include_program_output);
  ("title", Title_widget.set_title);
  ("breadcrumbs", Breadcrumbs_widget.breadcrumbs);
  ("footnotes", Footnotes_widget.footnotes);
  ("toc", Toc_widget.toc);
  ("preprocess_element", Inclusion_widgets.preprocess_element);
  ("relative_links", Link_widgets.relative_links);
  ("absolute_links", Link_widgets.absolute_links);
]
