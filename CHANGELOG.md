# libarchive

## 1.0.4.0

  * Add `noOpenCallback`

## 1.0.3.0

  * Fix types for `archive_set_read_callback` and
    `archive_read_set_seek_callback`

## 1.0.2.0

  * Add `Eq` instance for `ArchiveFormat`

## 1.0.1.0

  * Remove functions from libarchive 3.3.3

## 1.0.0.0

  * Get rid of `cbits`
  * Add low-level FFI bindings
  * Add high-level functions for unpacking archives

## 0.2.1.2

  * Stream from a file when using `unpackArchive`

## 0.2.1.1

  * Preserve modification times by default

## 0.2.1.0

  * Enable autodetection of archive format/compression
  * Slightly improved docs
  * Rename `unpackTarball` to `unpackArchive`

## 0.2.0.0

  * Fix bug in paths
