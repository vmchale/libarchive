module Codec.Archive
    ( unpackToDir
    , unpackArchive
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
import           System.Directory      (createDirectoryIfMissing,
                                        withCurrentDirectory)

done :: ReadResult -> Bool
done res = not (res == archiveOk || res == archiveRetry)

archiveFile :: FilePath -> IO (Ptr Archive)
archiveFile fp = withCString fp $ \cpath -> do
    a <- archive_read_new
    archive_read_support_format_all a
    archive_read_open_filename a cpath 10240
    pure a

unpackArchive' :: Ptr Archive -> IO ()
unpackArchive' a = do
    res <- getEntry a
    case res of
        Just x  -> archive_read_extract a x archiveExtractTime *> unpackArchive' a
        Nothing -> pure ()

getEntry :: Ptr Archive -> IO (Maybe (Ptr ArchiveEntry))
getEntry a = alloca $ \ptr -> do
    stop <- done <$> archive_read_next_header a ptr
    if stop
        then pure Nothing
        else Just <$> peek ptr

unpackArchive :: FilePath -- ^ Filepath pointing to archive
              -> FilePath -- ^ Filepath to unpack to
              -> IO ()
unpackArchive tarFp dirFp =
    withMissingDir dirFp $
        unpackArchive' =<< archiveFile tarFp

bsToArchive :: BS.ByteString -> IO (Ptr Archive)
bsToArchive bs = do
    a <- archive_read_new
    archive_read_support_format_all a
    useAsCStringLen bs $
        \(charPtr, sz) ->
            archive_read_open_memory a charPtr (fromIntegral sz)
    pure a

withMissingDir :: FilePath -> IO a -> IO a
withMissingDir fp act = do
    createDirectoryIfMissing True fp
    withCurrentDirectory fp act

unpackToDir :: FilePath -- ^ Directory to unpack in
            -> BS.ByteString -- ^ 'ByteString' containing archive
            -> IO ()
unpackToDir fp bs =
    withMissingDir fp $
        unpackArchive' =<< bsToArchive bs
