=========
Changelog
=========

0.2.0 (2023-01-09)
==================

* Added support for ``include`` statements, with the option to set the `include_path` in both
  :func:`.load` and :func:`.loads`.

* Swapped the internal Rust implementation to use ``dyn BufRead`` instead of being generic.  This
  reduced compile times and duplication of internal boiler plate in the PyO3 bindings.

0.1.0 (2023-01-08)
==================

* Initial release for CPython 3.8 to 3.11, on Linux i686, Linux x86_64, macOS x86_64, Windows x64
  and Windows x32.
