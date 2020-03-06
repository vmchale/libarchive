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
    , entriesToFileCpio
    , entriesToBS
    , entriesToBS7zip
    , entriesToBSzip
    , entriesToBSL
    , entriesToBSLzip
    , entriesToBSL7zip
    , entriesToBSLCpio
    , readArchiveFile
    , readArchiveBS
    , readArchiveBSL
    , packFiles
    , packFilesZip
    , packFiles7zip
    , packFilesCpio
    , packToFile
    , packToFileZip
    , packToFile7Zip
    , packToFileCpio
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

import           Codec.Archive.Monad
import           Codec.Archive.Pack
import           Codec.Archive.Pack.Lazy
import           Codec.Archive.Permissions
import           Codec.Archive.Types
import           Codec.Archive.Unpack
import           Codec.Archive.Unpack.Lazy
