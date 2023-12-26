# Libarchive

These are Haskell bindings to [libarchive](https://github.com/libarchive/libarchive).

This project is separated into two packages:

* `libarchive`: the Haskell bindings (this is what you want to depend on)
* `libarchive-clib`: bundled C sources to avoid depending on system libarchive
* `zlib-clib`: bundled C sources to avoid depending on system zlib

## Installing from Git

To build this package using Cabal directly from Git, you must run `autoreconf -i`
in the `libarchive-clib/` subdirectory before the usual Cabal build steps
(cabal {configure,build,install}). The program `autoreconf` is part of
[GNU autoconf](https://www.gnu.org/software/autoconf/). There is no need to
run the configure script: cabal configure will do this for you.

If you're using a `cabal.project` you can add the following section:

```
source-repository-package
  type: git
  location: https://github.com/vmchale/libarchive.git
  tag: <sha>
  subdir: libarchive
          libarchive-clib
          zlib-clib
  post-checkout-command: sh -c "cd libarchive-clib && autoreconf -i"
```


