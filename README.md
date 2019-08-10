soupault
========

Soupault is a static website generator based on HTML rewriting rather than template
processing. It is based on the [lambdasoup](http://aantron.github.io/lambdasoup/) library and named after
the French dadaist and surrealist poet Philippe Soupault.

In a startup pitch style, it's a website generator for Markdown haters.

Visit https://baturin.org/projects/soupault for documentation.

## Building from source

Soupault uses Lua-ML for plugin support, and that packag is not yet in the OPAM repository,
so you'll have to install it by hand for now:

```
opam pin add git+https://github.com/lindig/lua-ml
```

The rest should be just:

```
opam pin add soupault .
```

Doesn't build with OCaml 4.08 yet due to library dependencies.

### Building on/for Windows

As of fileutils 0.5.3, the fileutils library doesn't build or work on Windows.
Fixes for that are available in the upstream but are not yet in an OPAM release.

Since the upstream source is not OPAMified, you will need to install it by hand
from htts://github.com/gildor478/ocaml-fileutils


