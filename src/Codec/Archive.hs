module Codec.Archive
    ( -- * High-level functionality
      unpackToDir
    , unpackArchive
    -- * Types
    , Archive
    , ArchiveEntry
    -- * Lower-level API types
    , ReadResult
    , ExtractFlags
    ) where

import           Codec.Archive.Foreign
import           Codec.Archive.Types
import           Control.Applicative
import           Data.ByteString       (useAsCStringLen)
import qualified Data.ByteString       as BS
import           Foreign.C.String
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr           (Ptr)
import           Foreign.Storable      (Storable (..))
import           System.FilePath       ((</>))

archiveFile :: FilePath -> IO (Ptr Archive)
archiveFile fp = withCString fp $ \cpath -> do
    a <- archive_read_new
    archive_read_support_format_all a
    archive_read_open_filename a cpath 10240
    pure a

-- | Unpack an archive in a given directory
unpackEntriesFp :: Ptr Archive -> FilePath -> IO ()
unpackEntriesFp a fp = do
    res <- getEntry a
    case res of
        Nothing -> pure ()
        Just x  -> do
            preFile <- archive_entry_pathname x
            file <- peekCString preFile
            let file' = fp </> file
            withCString file' $ \fileC ->
                archive_entry_set_pathname x fileC
            archive_read_extract a x archiveExtractTime
            archive_entry_set_pathname x preFile
            unpackEntriesFp a fp

getEntry :: Ptr Archive -> IO (Maybe (Ptr ArchiveEntry))
getEntry a = alloca $ \ptr -> do
    let done res = not (res == archiveOk || res == archiveRetry)
    stop <- done <$> archive_read_next_header a ptr
    if stop
        then pure Nothing
        else Just <$> peek ptr

unpackArchive :: FilePath -- ^ Filepath pointing to archive
              -> FilePath -- ^ Filepath to unpack to
              -> IO ()
unpackArchive tarFp dirFp = do
    a <- archiveFile tarFp
    unpackEntriesFp a dirFp
    archive_read_free a

bsToArchive :: BS.ByteString -> IO (Ptr Archive)
bsToArchive bs = do
    a <- archive_read_new
    archive_read_support_format_all a
    useAsCStringLen bs $
        \(charPtr, sz) ->
            archive_read_open_memory a charPtr (fromIntegral sz)
    pure a

unpackToDir :: FilePath -- ^ Directory to unpack in
            -> BS.ByteString -- ^ 'ByteString' containing archive
            -> IO ()
unpackToDir fp bs = do
    a <- bsToArchive bs
    unpackEntriesFp a fp
    archive_read_free a
