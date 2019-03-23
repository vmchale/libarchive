module Codec.Archive.Unpack ( hsEntries
                            , unpackEntriesFp
                            ) where

import           Codec.Archive.Foreign
import           Codec.Archive.Types
import           Control.Monad         (void)
import qualified Data.ByteString       as BS
import           Foreign.C.String
import           Foreign.Marshal.Alloc (alloca, allocaBytes)
import           Foreign.Ptr           (Ptr)
import           Foreign.Storable      (Storable (..))
import           System.FilePath       ((</>))

readEntry :: Ptr Archive -> Ptr ArchiveEntry -> IO Entry
readEntry a entry =
    Entry
        <$> (peekCString =<< archive_entry_pathname entry)
        <*> readContents a entry
        <*> archive_entry_perm entry
        <*> readOwnership entry
        <*> readTimes entry

-- | Yield the next entry in an archive
getHsEntry :: Ptr Archive -> IO (Maybe Entry)
getHsEntry a = do
    entry <- getEntry a
    case entry of
        Nothing -> pure Nothing
        Just x  -> Just <$> readEntry a x

-- | Return a list of 'Entry's.
hsEntries :: Ptr Archive -> IO [Entry]
hsEntries a = do
    next <- getHsEntry a
    case next of
        Nothing -> pure []
        Just x  -> (x:) <$> hsEntries a

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
            void $ archive_read_extract a x archiveExtractTime
            archive_entry_set_pathname x preFile
            void $ archive_read_data_skip a
            unpackEntriesFp a fp

readBS :: Ptr Archive -> Int -> IO BS.ByteString
readBS a sz =
    allocaBytes sz $ \buff ->
        archive_read_data a buff (fromIntegral sz) *>
        BS.packCStringLen (buff, sz)

readContents :: Ptr Archive -> Ptr ArchiveEntry -> IO EntryContent
readContents a entry = go =<< archive_entry_filetype entry
    where go ft | ft == regular = NormalFile <$> (readBS a =<< sz)
                | ft == symlink = Symlink <$> (peekCString =<< archive_entry_symlink entry)
                | ft == directory = pure Directory
                | otherwise = error "Unsupported filetype"
          sz = fromIntegral <$> archive_entry_size entry

readOwnership :: Ptr ArchiveEntry -> IO Ownership
readOwnership entry =
    Ownership
        <$> (peekCString =<< archive_entry_uname entry)
        <*> (peekCString =<< archive_entry_gname entry)
        <*> archive_entry_uid entry
        <*> archive_entry_gid entry

readTimes :: Ptr ArchiveEntry -> IO ModTime
readTimes entry =
    (,) <$> archive_entry_mtime entry <*> archive_entry_mtime_nsec entry

-- | Get the next 'ArchiveEntry' in an 'Archive'
getEntry :: Ptr Archive -> IO (Maybe (Ptr ArchiveEntry))
getEntry a = alloca $ \ptr -> do
    let done res = not (res == archiveOk || res == archiveRetry)
    stop <- done <$> archive_read_next_header a ptr
    if stop
        then pure Nothing
        else Just <$> peek ptr

