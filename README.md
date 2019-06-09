soupault
========

Soupault is a static website generator based on HTML rewriting rather than template
processing. It is based on the [lambdasoup](http://aantron.github.io/lambdasoup/) library and named after
the French dadaist and surrealist poet Philippe Soupault.

In a startup pitch style, it's a website generator for Markdown haters.

# Motivation

There are lots of static website generators around already, and most of them are variations on
"take a Markdown file, convert to HTML, and feed to a template processor".

In practice, limitations of Markdown make people innvent ad hoc extensions or just mix Markdown with HTML,
which defeats the purpose of Markdown. The need to store metadata such as page titles leads to Frankenstein
formats that mix YAML headers with Markdown. That solution can work, but can there be any alternatives?

Any webmaster will learn to write HTML eventually, most of the inconvenience of having to close tags etc.
can be offset with a good editor, and as many client-side scripts and microformats showed, HTML elements
and attributes can store metadata just fine.

HTML rewriting can also do things that are hard or impossible to do with templates, such as deleting something
from a page if needed rather than just adding new content to a template.

If anything, a Markdown/RST/whatever preprocessor can be plugged into the pipeline.

# Design goals

* Do not use any special syntax other than HTML in templates and page files
* Make it easy to create arbitrarily nested website structure
* Provide built-in functionality for common tasks

Right now soupault is a prototype and does not provide all that yet.

# Usage

## Directory structure

Website content is stored in a directory referred to as `site_dir`. By default it's `site/`.
Every subdirectory is a section. Every subdirectory of a subdirectory is a subsection and so on.

Soupault's behaviour and settings are controlled by a config file names `soupault.conf`.

Templates are stored in a directory named `templates/` by convention. They can be stored anywhere,
but with default configuration soupault will look for `templates/main.html` file to use as a default template.

You can fine a real example in the `sample-site` directory here:

```
sample-site/
├── site
│   ├── about.inc
│   └── index.inc
├── templates
│   └── main.html
└── soupault.conf
```

At build time, soupault will produce something like this from it:

```
build/
├── about
│   └── index.html
└── index.html

```

## Creating page templates

Complete pages are created for page skeletons, that is, HTML pages with all container tags left empty.
Because they are processed by HTML rewriting rather than fed to a template processor, I avoid calling them templates.
Since there's no template processor, there's no special syntax for elements to be replaced.
The system simply inserts new HTML content into elements with certain selectors.

This is what the minimum viable page skeleton looks like:

```
<html>
  <head> </head>
  <body>
  </body>
</html>
```

By default, soupault will insert page content into the `<body>` element. You can override it using the
`content_selector` option in the config. You can use any valid CSS3 selector.

For example, this will make soupault insert the content into the element with `id="content"`:
```
[settings]
  content_selector = "#content"
```

# TODO

* Widget dispatch mechanism
* Built-in widgets (include, exec, title, breadcrumbs, TOC...)
* Per-section and per-page settings overrides
* Generated page caching
* Page preprocessors
