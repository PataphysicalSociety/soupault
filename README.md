soupault
========

![maintenance-status](https://img.shields.io/badge/maintenance-actively--developed-brightgreen.svg)
![Build](https://github.com/PataphysicalSociety/soupault/actions/workflows/main.yml/badge.svg)
![GitHub all releases](https://img.shields.io/github/downloads/PataphysicalSociety/soupault/total)

Soupault is an HTML manipulation tool. It can be a static site generator
or an HTML (post-)processor for existing websites and allows you to define your own content model
and page processing rules using built-in actions, external executables, templates, or Lua plugins.

Soupault works with the HTML element tree of the page, so it can do many things that traditionally could be done with client-side JS:
inject new HTML into existing complete pages, create a table of contents that respects and uses `id`'s of HTML headings and more.

It also doesn't use front matter but extracts metadata from HTML instead: you tell it what to extract using CSS3 selectors,
so even hand-written static pages can be indexed rather than treated as opaque assets.

For example, here's what a content model for a blog may look like:

```toml
# Post title
[index.fields.title]
  # Try to find <h1 id="post-title">,
  # else use the first <h1> 
  selector = ["h1#post-title", "h1"]

  # Fail the build if post title cannot be found
  required = true

# Post excerpt
[index.fields.excerpt]
  # Use <p id="post-excerpt"> if a page has it,
  # else use the first paragraph.
  # This allows using a paragraph other than the first one
  # as the post excerpt.
  selector = ["p#post-excerpt", "p"]

# Post date
[index.fields.date]
  selector = ["time#post-date", "time"]

  # Extract the datetime="" attribute from the <time> element,
  # if it's set.
  extract_attribute = "datetime"

  # If there's no datetime attribute in <time>,
  # then use that element's content
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

* Durable and easy to upgrade or roll back: it's available as a statically-linked binary with no dependencies.
* Extensible: you can bring your own [page preprocessors](https://soupault.net/reference-manual/#page-preprocessors)
  (e.g., Markdown to HTML convertors), define [asset processors](https://soupault.net/reference-manual/#asset-processing)
  (e.g., a Sass/Less compiler, an image optimizer),
  pipe HTML elements through [external programs](https://soupault.net/reference-manual/#preprocess-element-widget),
  and load [Lua plugins](https://soupault.net/plugins/).
* Flexible: most options are configurable, most built-in features can be reimplemented as Lua plugins,
  and there are [page processing hooks](https://soupault.net/reference-manual/#page-processing-hooks).

Soupault is named after [Philippe Soupault](https://en.wikipedia.org/wiki/Philippe_Soupault),
a French dadaist and surrealist writer and poet,
because it uses [lambdasoup](http://aantron.github.io/lambdasoup/) library to work with tag soups.

Visit [soupault.net](https://www.soupault.net) for details.

For support and discussion, write a message to the [mailing list](https://lists.sr.ht/~dmbaturin/soupault).

# Installation

Prebuilt binaries are available for Linux (x86-64 and Aarch64), Windows, and macOS (x86-64).
You can download them from https://files.baturin.org/software/soupault
and from GitHub releases (https://github.com/PataphysicalSociety/soupault/releases).

You can verify release archive integrity using this minisign key:

```
minisign -VP RWRfW+gkhk/+iA7dOUtTio6G6KeJCiAEp4Zfozw7eqv2shN90+5z20Cy -m <file>
```

You can also install stable release versions from [OPAM](https://opam.ocaml.org):

```sh
opam install soupault
```

Finally, you can build the latest development version with:

```sh
opam pin add git+https://github.com/PataphysicalSociety/soupault
```

To build static binaries, you need to install OCaml with musl runtime,
then use the `static` Dune profile:

```
# For OCaml 5.3.0, adjust for your desired version
opam switch create 5.3.0-musl ocaml-variants.5.3.0+options ocaml-option-musl ocaml-option-static
opam switch 5.3.0-musl

# Build static binaries
dune build --profile=static
```

# Contributing

Bug reports and patches are always welcome. Feature requests and new features are also welcome,
but please consider discussing them with the maintainer first.

You can contribute either through [GitHub](https://github.com/PataphysicalSociety/soupault)
or through [Codeberg](https://codeberg.org/PataphysicalSociety/soupault).
