soupault
========

Soupault is a static website generator based on HTML rewriting rather than template
processing. It is based on the [lambdasoup](http://aantron.github.io/lambdasoup/) library and named after
the French dadaist and surrealist poet Philippe Soupault.

In a startup pitch style, it's a website generator for Markdown haters.

Visit https://baturin.org/projects/soupault for documentation.

## Building from source

Soupault uses Lua-ML for plugin support, and that package is not yet in the OPAM repository,
so you'll have to install it by hand for now:

```
opam pin add git+https://github.com/lindig/lua-ml
```

The rest should be just:

```
opam pin add soupault .
```


