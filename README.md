soupault
========

![Build](https://github.com/dmbaturin/soupault/actions/workflows/main.yml/badge.svg)
![Lines of code](https://img.shields.io/tokei/lines/github/dmbaturin/soupault)
![GitHub all releases](https://img.shields.io/github/downloads/dmbaturin/soupault/total)

Soupault is an HTML manipulation tool. It can be any of:

* static site generator
* HTML processor
* metadata extractor

or all of them at the same time.

Soupault works with the HTML element tree of the page, so it can do many things that traditionally could be done with client-side JS:
inject new HTML into existing complete pages, create a table of contents that preserves the `id` elements of HTML headings and more.

It also doesn't use front matter and extracts metadata from HTML instead, using a CSS3 selector to metadata field mapping,
so even hand-written static pages can be indexed rather than treated as assets. For example:

```toml
[index.fields.title]
  # Try to find <h1 id="post-title"> if it exists,
  # else use the first <h1> 
  selector = ["h1#post-title", "h1"]

[index.fields.excerpt]
  selector = ["p#post-excerpt", "p"]

[index.fields.date]
  selector = ["time#post-date", "time"]
  extract_attribute = "datetime"
  fallback_to_content = true
```

Extracted metadata can then be rendered and injected into pages:

```toml
[index.views.blog]
  # Insert rendered data into the element that matches "#blog-index" CSS selector.
  index_selector = "#blog-index"
  index_item_template = """
    <h2><a href="{{url}}">{{title}}</a></h2>
    <p><strong>Last update:</strong> {{date}}.</p>
    <p>{{excerpt}}</p>
    <a href="{{url}}">Read more</a>
  """
```

Soupault is...

* Eternal: it comes as a statically-linked binary with no dependencies.
* Extensible: you can bring your own [page preprocessors](https://soupault.app/reference-manual/#page-preprocessors) (e.g. Markdown to HTML convertors), pipe HTML elements through [external programs](https://soupault.app/reference-manual/#preprocess-element-widget), and load [Lua plugins](https://soupault.app/plugins/).
* Flexible: most options are configurable and most built-in features can be reimplemented as Lua plugins.

Soupault is named after the French dadaist and surrealist writer Philippe Soupault
because it's based on the [lambdasoup](http://aantron.github.io/lambdasoup/) library.

Visit https://www.soupault.app for details.

For support and discussion, write a message to the [mailing list](https://lists.sr.ht/~dmbaturin/soupault).

# Installation

Pre-built binaries are available for Linux, Windows, and macOS. You can download them from https://files.baturin.org/software/soupault
and from Github releases (https://github.com/dmbaturin/soupault/releases).

You can verify release archive integrity using this signify/minisign key: `RWRfW+gkhk/+iA7dOUtTio6G6KeJCiAEp4Zfozw7eqv2shN90+5z20Cy`.

You can also install stable release versions from [OPAM](https://opam.ocaml.org):

```sh
opam install soupault
```

Finally, you can build the latest development version with:

```sh
opam pin add git+https://github.com/dmbaturin/soupault
```

# Contributing

Bug reports and patches are always welcome. Feature requests and new features are also welcome,
but please consider discussing them with the maintainer first.
