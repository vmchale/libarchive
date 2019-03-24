-- | This module contains higher-level functions for working with archives in
-- Haskell. See "Codec.Archive.Foreign" for direct bindings to
-- libarchive.
module Codec.Archive
    ( -- * High-level functionality
      unpackToDir
    , unpackArchive
    , entriesToFile
    , entriesToFileZip
    , entriesToFile7Zip
    , entriesToBS
    , entriesToBS7zip
    , entriesToBSzip
    , readArchiveFile
    , readArchiveBS
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

import           Codec.Archive.Foreign
import           Codec.Archive.Pack
import           Codec.Archive.Types
import           Codec.Archive.Unpack
import           Control.Monad         (void, (<=<))
import           Data.ByteString       (useAsCStringLen)
import qualified Data.ByteString       as BS
import           Foreign.C.String
import           Foreign.Ptr           (Ptr)
import           System.IO.Unsafe      (unsafePerformIO)

-- | Read from an 'Archive' and then free it
actFree :: (Ptr Archive -> IO a) -> Ptr Archive -> IO a
actFree fact a = fact a <* archive_read_free a

-- | Read an archive from a file. The format of the archive is automatically
-- detected.
readArchiveFile :: FilePath -> IO [Entry]
readArchiveFile = actFree hsEntries <=< archiveFile

-- | Read an archive contained in a 'BS.ByteString'. The format of the archive is
-- automatically detected.
readArchiveBS :: BS.ByteString -> [Entry]
readArchiveBS = unsafePerformIO . (actFree hsEntries <=< bsToArchive)
{-# NOINLINE readArchiveBS #-}

archiveFile :: FilePath -> IO (Ptr Archive)
archiveFile fp = withCString fp $ \cpath -> do
    a <- archive_read_new
    void $ archive_read_support_format_all a
    void $ archive_read_open_filename a cpath 10240
    pure a

-- | This is more efficient than
--
-- @
-- unpackToDir "llvm" =<< BS.readFile "llvm.tar"
-- @
unpackArchive :: FilePath -- ^ Filepath pointing to archive
              -> FilePath -- ^ Dirctory to unpack in
              -> IO ()
unpackArchive tarFp dirFp = do
    a <- archiveFile tarFp
    unpackEntriesFp a dirFp
    void $ archive_read_free a

bsToArchive :: BS.ByteString -> IO (Ptr Archive)
bsToArchive bs = do
    a <- archive_read_new
    void $ archive_read_support_format_all a
    useAsCStringLen bs $
        \(charPtr, sz) ->
            void $ archive_read_open_memory a charPtr (fromIntegral sz)
    pure a

unpackToDir :: FilePath -- ^ Directory to unpack in
            -> BS.ByteString -- ^ 'BS.ByteString' containing archive
            -> IO ()
unpackToDir fp bs = do
    a <- bsToArchive bs
    unpackEntriesFp a fp
    void $ archive_read_free a
