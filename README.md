soupault
========

Soupault is an HTML manipulation tool. It can be any of:

* static site generator
* HTML processor
* metadata extractor

or all of them at the same time.

It builds on the idea that HTML is a machine-readable format.

Client-side JavaScript has always been used to manipulate pages in-browser.
For manipulating pages on disk, people traditionally used template processors.
Soupault can parse an HTML page into an element tree, manipulate elements, and save the result to disk.

Web scrapers have been used for extracting data from someone else's pages. Microformats have been used
to let other people know what to extract.
For their own pages, people usually used "front matter".
Soupault allows you to define your own "microformats" on the fly. For example, automatically use
the first `<h1>`, or `<h1 id="title">` for the page `<title>`. You can define your own fields
based on CSS selectors and export the index to JSON, then make a HTML page with a blog archive
or an RSS/Atom/JSONFeed from it.

Static site generators have been either easily extensible but written in interpreted languages
or shipped as static binaries but self-contained.
Soupault is an easy to install static binary, but it embeds a Lua interpreter that has access to
the page element tree. Much like the DOM API for JS, but for Lua.

It's also friendly to existing websites. Clean URLs are optional. Assembling pages from a template
and a body is also optional: if you page has an `<html>` element, it's excluded from the assembly stage.
You can disable "templating", or mix unique and templated pages.

Soupault is named after the French dadaist and surrealist writer Philippe Soupault
because it's based on the [lambdasoup](http://aantron.github.io/lambdasoup/) library.

Visit https://soupault.neocities.org for details.

For support and discussion, write a message to the [mailing list](https://lists.sr.ht/~dmbaturin/soupault).

# Installation

Pre-built binaries are available for Linux, Windows, and mac OS. You can download them via [soupault.neocities.org](https://soupault.neocities.org/#downloads)
and from Github releases.

You can verify the binaries using this signify/minisign key: `RWRfW+gkhk/+iA7dOUtTio6G6KeJCiAEp4Zfozw7eqv2shN90+5z20Cy`.

You can also install stable release versions from [OPAM](https://opam.ocaml.org):

```
opam install soupault
```

Finally, you can build the latest development version with:

```
opam pin add git+https://github.com/dmbaturin/soupault
```

# Contributing

Bug reports and patches are always welcome. Feature requests and new features are also welcome,
but please consider discussing them with the maintainer first.

You can submit patches either as Github pull requests or send them to the Sourcehut mailing list.
