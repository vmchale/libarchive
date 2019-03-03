module Codec.Archive
    ( unpackToDir
    , unpackArchive
    ) where

import           Codec.Archive.Foreign
import           Codec.Archive.Types
import           Control.Applicative
import           Control.Monad         (unless)
import           Data.ByteString       (useAsCStringLen)
import qualified Data.ByteString       as BS
import           Foreign.C.String
import           Foreign.Marshal.Alloc (free, malloc)
import           Foreign.Ptr           (Ptr)
import           Foreign.Storable      (Storable (..))
import           System.FilePath       (pathSeparator, (</>))

getPtr :: Storable a => a -> IO (Ptr a)
getPtr ptr = do
    newPtr <- malloc
    poke newPtr ptr
    pure newPtr

archiveCond :: Ptr Archive
            -> Ptr ArchiveEntry
            -> IO Bool
archiveCond a entry = do
    ptrPtr <- getPtr entry
    res <- archive_read_next_header a ptrPtr
    free ptrPtr
    pure $ res == archiveOk || res == archiveRetry

common :: FilePath -- ^ Directory name
       -> Ptr Archive
       -> Ptr ArchiveEntry
       -> IO ()
common dirname a entry = loop *> archive_read_free a
    where
        loop = do
            done <- not <$> archiveCond a entry
            unless done $ do
                prePathName <- archive_entry_pathname entry
                -- FIXME: segfaults here b/c prePathName is null
                prePathNameHs <- peekCString prePathName
                let fp = dirname </> prePathNameHs
                withCString fp $ \fpc -> do
                    archive_entry_set_pathname entry fpc
                    archive_read_extract a entry archiveExtractTime
                    archive_entry_set_pathname entry prePathName
                    archive_read_data_skip a
                    loop

unpackArchive :: FilePath -- ^ Filepath pointing to archive
              -> FilePath -- ^ Filepath to unpack to
              -> IO ()
unpackArchive tarFp dirFp = withCString tarFp $ \tarFpc -> do
    entry <- archive_entry_new
    a <- archive_read_new
    archive_read_support_format_all a
    archive_read_open_filename a tarFpc 10240
    common dirFp a entry

unpackToDir :: FilePath -- ^ Directory to unpack in
            -> BS.ByteString -- ^ 'ByteString' containing archive
            -> IO ()
unpackToDir fp bs = do
    fp' <- newCString (fp ++ [pathSeparator])
    useAsCStringLen bs $
        \(charPtr, sz) ->
            unpack_in_dir fp' charPtr (fromIntegral sz)
