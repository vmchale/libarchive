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
    , entriesToFileXar
    , entriesToFileShar
    , entriesToBS
    , entriesToBS7zip
    , entriesToBSzip
    , entriesToBSL
    , entriesToBSLzip
    , entriesToBSL7zip
    , entriesToBSLCpio
    , entriesToBSLXar
    , entriesToBSLShar
    , readArchiveFile
    , readArchiveBS
    , readArchiveBSL
    , packFiles
    , packFilesZip
    , packFiles7zip
    , packFilesCpio
    , packFilesXar
    , packFilesShar
    , packToFile
    , packToFileZip
    , packToFile7Zip
    , packToFileCpio
    , packToFileXar
    , packToFileShar
    -- * Concrete (Haskell) types
    , ArchiveResult (..)
    , ArchiveEntryDigest (..)
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
    , throwArchiveM
    -- * Permissions helpers
    , standardPermissions
    , executablePermissions
    ) where

import           Codec.Archive.Internal.Monad
import           Codec.Archive.Internal.Pack
import           Codec.Archive.Internal.Pack.Lazy
import           Codec.Archive.Internal.Unpack
import           Codec.Archive.Internal.Unpack.Lazy
import           Codec.Archive.Permissions
import           Codec.Archive.Types
