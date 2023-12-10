# Libarchive

These are Haskell bindings to [libarchive](https://github.com/libarchive/libarchive).

This project is separated into two packages:

* `libarchive`: the Haskell bindings (this is what you want to depend on)
* `libarchive-clib`: bundled C sources to avoid depending on system libarchive
* `zlib-clib`: bundled C sources to avoid depending on system zlib

