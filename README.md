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

`libarchive` is faster than `tar` when unpacking archives.

```
benchmarking roundtrip/libarchive
time                 308.2 μs   (305.2 μs .. 311.4 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 314.4 μs   (311.8 μs .. 317.6 μs)
std dev              9.721 μs   (7.337 μs .. 13.78 μs)
variance introduced by outliers: 25% (moderately inflated)

benchmarking roundtrip/tar
time                 341.1 μs   (337.2 μs .. 345.6 μs)
                     0.998 R²   (0.995 R² .. 0.999 R²)
mean                 341.1 μs   (337.6 μs .. 347.2 μs)
std dev              15.62 μs   (10.13 μs .. 24.01 μs)
variance introduced by outliers: 42% (moderately inflated)

benchmarking unpack/libarchive (via bytestring)
time                 902.6 μs   (887.2 μs .. 920.0 μs)
                     0.995 R²   (0.988 R² .. 0.999 R²)
mean                 924.5 μs   (911.9 μs .. 952.4 μs)
std dev              59.38 μs   (32.00 μs .. 104.6 μs)
variance introduced by outliers: 53% (severely inflated)

benchmarking unpack/libarchive (C API)
time                 855.7 μs   (832.9 μs .. 897.5 μs)
                     0.991 R²   (0.978 R² .. 1.000 R²)
mean                 854.4 μs   (845.7 μs .. 874.2 μs)
std dev              40.97 μs   (16.99 μs .. 83.85 μs)
variance introduced by outliers: 39% (moderately inflated)

benchmarking unpack/tar
time                 1.776 ms   (1.729 ms .. 1.843 ms)
                     0.994 R²   (0.985 R² .. 0.999 R²)
mean                 1.767 ms   (1.752 ms .. 1.798 ms)
std dev              70.54 μs   (39.81 μs .. 126.7 μs)
variance introduced by outliers: 27% (moderately inflated)
```

