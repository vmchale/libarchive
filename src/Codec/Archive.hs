-- | This module contains higher-level functions for working with archives in
-- Haskell. See "Codec.Archive.Foreign" for direct bindings to
-- libarchive.
module Codec.Archive
    ( -- * High-level functionality
      unpackToDir
    , unpackToDirLazy
    , unpackArchive
    , entriesToFile
    , entriesToFileZip
    , entriesToFile7Zip
    , entriesToBS
    , entriesToBS7zip
    , entriesToBSzip
    , readArchiveFile
    , readArchiveBS
    , readArchiveBSL
    -- * Concrete (Haskell) types
    , Entry (..)
    , EntryContent (..)
    , Ownership (..)
    , Permissions
    , ModTime
    , Id
    -- * Permissions helpers
    , standardPermissions
    , executablePermissions
    ) where

import           Codec.Archive.Pack
import           Codec.Archive.Types
import           Codec.Archive.Unpack
import           Codec.Archive.Unpack.Lazy
