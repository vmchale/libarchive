# libarchive

[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/libarchive/badge)](https://matrix.hackage.haskell.org/package/libarchive)
[![Hackage](https://img.shields.io/hackage/v/libarchive.svg)](http://hackage.haskell.org/package/libarchive)
[![Dependencies of latest version on Hackage](https://img.shields.io/hackage-deps/v/libarchive.svg)](https://hackage.haskell.org/package/libarchive)

This contains Haskell bindings to
[libarchive](http://libarchive.org/). It was created as an alternative to
[tar](http://hackage.haskell.org/package/tar) and
[tar-conduit](http://hackage.haskell.org/package/tar-conduit), but it supports
more archive formats.

It has a high-level Haskell API for creating and unpacking archives in addition
to the C API. Like the `tar` package, it can stream from lazy `ByteString`s.

## Hacking

To run the test suite, first run

```
make
```

to download test data.

## Performance

`libarchive` is faster than `tar` or `tar-conduit` when unpacking archives.

```
benchmarking roundtrip/libarchive
time                 248.5 μs   (247.0 μs .. 250.0 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 249.7 μs   (248.2 μs .. 251.6 μs)
std dev              5.637 μs   (4.385 μs .. 8.012 μs)
variance introduced by outliers: 15% (moderately inflated)

benchmarking roundtrip/tar
time                 322.7 μs   (321.9 μs .. 323.7 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 324.0 μs   (322.9 μs .. 325.1 μs)
std dev              3.673 μs   (2.837 μs .. 5.119 μs)

benchmarking unpack/libarchive (via bytestring)
time                 1.146 ms   (1.133 ms .. 1.157 ms)
                     0.999 R²   (0.997 R² .. 0.999 R²)
mean                 1.110 ms   (1.096 ms .. 1.156 ms)
std dev              72.18 μs   (30.15 μs .. 141.7 μs)
variance introduced by outliers: 51% (severely inflated)

benchmarking unpack/libarchive (C API)
time                 1.009 ms   (994.1 μs .. 1.022 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 1.002 ms   (999.0 μs .. 1.006 ms)
std dev              11.81 μs   (8.603 μs .. 18.76 μs)

benchmarking unpack/tar
time                 3.600 ms   (3.271 ms .. 4.001 ms)
                     0.939 R²   (0.898 R² .. 0.977 R²)
mean                 4.119 ms   (3.814 ms .. 5.295 ms)
std dev              1.631 ms   (541.5 μs .. 3.272 ms)
variance introduced by outliers: 98% (severely inflated)

benchmarking unpack/tarConduit
time                 4.946 ms   (4.072 ms .. 6.308 ms)
                     0.835 R²   (0.779 R² .. 0.988 R²)
mean                 4.164 ms   (3.967 ms .. 4.574 ms)
std dev              848.0 μs   (442.7 μs .. 1.620 ms)
variance introduced by outliers: 88% (severely inflated)
```
