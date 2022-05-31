# 4.0.1

## Bug fixes

* Pages that produce completely empty index entries (no defined fields) no longer cause an internal error (github#45, report by github/@akavel).
* With `clean_urls = false`, pages now have correct url index fields (with original extension removed) (github#44, report by github/@laumann).

# 4.0.0

## Breaking changes

* Index views no longer have default rendering and require `index_template`, `index_item_template`, or `index_processor`.
* `absolute_links` widgets without `prefix` are considered configuration errors.
* Empty `settings.build_dir` is no longer allowed (formerly caused soupault to use the current dir as output dir).

## New features

* Index fields can now be marked as required (missing fields in pages will cause build errors).
* New `settings.process_pages_first` option allows the user to force processing certain pages before all other.
* Lua code can now take over certain tasks via page processing hooks: `pre-parse`, `pre-process`, `post-index`, `render`, and `save`.
* Index processors can now be written in Lua.
* Lua index processors can inject new pages into the queue (for pagination, taxonomies, and so on).
* New `index.index_first` option allows pages to access their own index entries (at cost of a two-pass workflow with a reduced index-only run done first).
* Index sorting options (`sort_by` and `sort_type`) can be specified in every index view independently.
* Index views now support `action` option to control generated HTML insertion.
* Lua plugins code can now be inlinded in the config using `lua_source` option.
* `relative_links` and `absolute_links` widgets support almost all imaginable that can contain URLs in attributes.
* New `--dump-index-json <file>` option allows exporting metadata to JSON without setting `index.dump_json` in the config.

## New Lua plugin API functions and variables

### New variables

* `target_file` (path to the output file, relative to the current working directory).
* `index_entry` (the index entry of the page being processed if `index.index_first = true`, otherwise it's `nil`).

### New functions

* `String.slugify_soft(string)` replaces all whitespace with hyphens, but doesn't touch any other characters.
* `String.url_encode(str)` and `String.url_decode(str)` for working with percent-encoded URLs.
* `String.join_url` as an alias for `String.join_path_unix`.
* `HTML.to_string(etree)` and `HTML.pretty_print(etree)` return string representations of element trees, useful for save hooks.
* `HTML.create_document()` creates an empty element tree.
* `HTML.clone_document(etree)` make a copy of a complete element tree.
* `HTML.append_root(etree, node)` adds a node after the last element.
* `HTML.child_count(elem)` returns the number of children of an element.
* `HTML.unwrap(elem)` yanks the child elements out of a parent and inserts them in its former place.
* `Table.take(table, limit)` removes up to `limit` items from a table and returns them.
* `Table.chunks(table, size)` splits a table into chunks of up to `size` items.
* `Table.has_value(table, value)` returns true if `value` is present in `table`.
* `Table.apply_to_values(func, table)` applies function `func` to every value in `table` (a simpler version of `Table.apply` if you don't care about keys).
* `Table.get_nested(table, {"key", "sub-key", ...})` and `Table.get_nested_default(table, {"key", "sub-key", ...}, default)` for easily retrieving values from nested tables (handy for getting config options).
* `Table.keys(table)` returns a list of all keys in `table`.
* `Sys.list_dir(path)` returns a list of all files in `path`.
* `Value.repr(value)` returns a string representation of a Lua value for debug output (similar to Python's `__repr__` in spirit).

### Unicode string functions

* `String.length` is now Unicode-aware, the old ASCII-only version is now available as `String.length_ascii`
* `String.truncate` is now Unicode-aware, the old ASCII-only version is now available as `String.truncate_ascii`

## Bug fixes

* The `numeric` index entry sorting method works correctly again.
* The `include_subsections` option is now available to all widgets and documented (it makes `section` apply to a dir and all its subdirs).
* Nested inline tables in `soupault.toml` work correctly now (fixed in [OTOML 1.0.1](https://github.com/dmbaturin/otoml/releases/tag/1.0.1)).
* Fixed an unhandled exception when handling misconfigured index views.
* Fixed a possible unhandled exception during page processing.
* `Sys.list_dir` correctly handles errors when the argument is not a directory.

## Behaviour changes

* If `index.sort_by` is not set, entries are now sorted by their `url` field rather than displayed in arbitrary order.
* Newlines are now inserted after HTML doctype declarations.
* Index entries are no longer sorted twice, so performance may be marginally better.
* The ToC widget now logs ignored headings in debug mode, to make it easier to debug configuration issues.
* Internal errors now produce a message that tells the user how to enable debug output and get an exception trace, if it's not enabled. I hope this feature goes unused. ;)
* Preprocessor commands are now quoted in debug logs for better readability (e.g. `running preprocessor "cmark --unsafe --smart"...`).
* Many debug log messages were improved and clarified.

 
