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
    , entriesToBSL
    , entriesToBSLzip
    , entriesToBSL7zip
    , readArchiveFile
    , readArchiveBS
    , readArchiveBSL
    , packFiles
    , packFilesZip
    , packFiles7zip
    , packToFile
    , packToFileZip
    , packToFile7Zip
    -- * Concrete (Haskell) types
    , ArchiveResult (..)
    , Entry (..)
    , Symlink (..)
    , EntryContent (..)
    , Ownership (..)
    , Permissions
    , ModTime
    , Id
    -- * Archive monad
    , ArchiveM
    , runArchiveM
    -- * Permissions helpers
    , standardPermissions
    , executablePermissions
    ) where

import           Codec.Archive.Internal.Monad
import           Codec.Archive.Internal.Pack
import           Codec.Archive.Internal.Pack.Lazy
import           Codec.Archive.Internal.Permissions
import           Codec.Archive.Internal.Types
import           Codec.Archive.Internal.Unpack
import           Codec.Archive.Internal.Unpack.Lazy
