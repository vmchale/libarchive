module Codec.Archive.Pack ( packEntries
                          , entriesToFile
                          , entriesToBS
                          , entriesToBSzip
                          , entriesToBS7zip
                          ) where

import           Codec.Archive.Foreign
import           Codec.Archive.Types
import           Control.Monad         (void)
import           Data.ByteString       (packCStringLen, useAsCStringLen)
import qualified Data.ByteString       as BS
import           Data.Foldable         (traverse_)
import           Data.Semigroup        (Sum (..), (<>))
import           Foreign.C.String
import           Foreign.Marshal.Alloc (alloca, allocaBytes)
import           Foreign.Ptr           (Ptr)
import           Foreign.Storable      (peek)
import           System.IO.Unsafe      (unsafePerformIO)

contentAdd :: EntryContent -> Ptr Archive -> Ptr ArchiveEntry -> IO ()
contentAdd (NormalFile contents) a entry = do
    archive_entry_set_filetype entry regular
    archive_entry_set_size entry (fromIntegral (BS.length contents))
    void $ archive_write_header a entry
    useAsCStringLen contents $ \(buff, sz) ->
        void $ archive_write_data a buff (fromIntegral sz)
contentAdd Directory a entry = do
    archive_entry_set_filetype entry directory
    void $ archive_write_header a entry
contentAdd (Symlink fp) a entry = do
    archive_entry_set_filetype entry symlink
    withCString fp $ \fpc ->
        archive_entry_set_symlink entry fpc
    void $ archive_write_header a entry

setOwnership :: Ownership -> Ptr ArchiveEntry -> IO ()
setOwnership (Ownership uname gname uid gid) entry =
    withCString uname $ \unameC ->
    withCString gname $ \gnameC ->
    sequence_
        [ archive_entry_set_uname entry unameC
        , archive_entry_set_gname entry gnameC
        , archive_entry_set_uid entry uid
        , archive_entry_set_gid entry gid
        ]

setTime :: ModTime -> Ptr ArchiveEntry -> IO ()
setTime (time', nsec) entry = archive_entry_set_mtime entry time' nsec

packEntries :: (Foldable t) => Ptr Archive -> t Entry -> IO ()
packEntries a = traverse_ (archiveEntryAdd a)

-- Get a number of bytes appropriate for creating the archive.
entriesSz :: (Foldable t, Integral a) => t Entry -> a
entriesSz = getSum . foldMap (Sum . entrySz)
    where entrySz e = 512 + 512 * (contentSz (content e) `div` 512 + 1)
          contentSz (NormalFile str) = fromIntegral $ BS.length str
          contentSz Directory        = 0
          contentSz (Symlink fp)     = fromIntegral $ length fp

-- | Returns a 'ByteString' containing a tar archive with the 'Entry's
entriesToBS :: Foldable t => t Entry -> BS.ByteString
entriesToBS = unsafePerformIO . entriesToBSGeneral archive_write_set_format_pax_restricted
{-# NOINLINE entriesToBS #-}

-- | Returns a 'ByteString' containing a @.7z@ archive with the 'Entry's
entriesToBS7zip :: Foldable t => t Entry -> BS.ByteString
entriesToBS7zip = unsafePerformIO . entriesToBSGeneral archive_write_set_format_7zip
{-# NOINLINE entriesToBS7zip #-}

-- | Returns a 'ByteString' containing a zip archive with the 'Entry's
entriesToBSzip :: Foldable t => t Entry -> BS.ByteString
entriesToBSzip = unsafePerformIO . entriesToBSGeneral archive_write_set_format_zip
{-# NOINLINE entriesToBSzip #-}

entriesToBSGeneral :: (Foldable t) => (Ptr Archive -> IO ArchiveError) -> t Entry -> IO BS.ByteString
entriesToBSGeneral modifier hsEntries' = do
    a <- archive_write_new
    void $ modifier a
    alloca $ \used ->
        allocaBytes bufSize $ \buffer -> do
            void $ archive_write_open_memory a buffer bufSize used
            packEntries a hsEntries'
            void $ archive_write_close a
            usedSz <- peek used
            res <- curry packCStringLen buffer (fromIntegral usedSz)
            void $ archive_write_free a
            pure res

    where bufSize :: Integral a => a
          bufSize = entriesSz hsEntries'

-- | Write some entries to a file, creating a tar archive. This is more
-- efficient than
--
-- @
-- BS.writeFile "file.tar" (entriesToBS entries)
-- @
entriesToFile :: Foldable t => FilePath -> t Entry -> IO ()
entriesToFile = entriesToFileGeneral archive_write_set_format_pax_restricted
-- this is the recommended format; it is a tar archive

entriesToFileGeneral :: Foldable t => (Ptr Archive -> IO ArchiveError) -> FilePath -> t Entry -> IO ()
entriesToFileGeneral modifier fp hsEntries' = do
    a <- archive_write_new
    void $ modifier a
    withCString fp $ \fpc ->
        void $ archive_write_open_filename a fpc
    packEntries a hsEntries'
    void $ archive_write_free a

withArchiveEntry :: (Ptr ArchiveEntry -> IO a) -> IO a
withArchiveEntry fact = do
    entry <- archive_entry_new
    res <- fact entry
    archive_entry_free entry
    pure res

archiveEntryAdd :: Ptr Archive -> Entry -> IO ()
archiveEntryAdd a (Entry fp contents perms owner mtime) =
    withArchiveEntry $ \entry -> do
        withCString fp $ \fpc ->
            archive_entry_set_pathname entry fpc
        archive_entry_set_perm entry perms
        setOwnership owner entry
        setTime mtime entry
        contentAdd contents a entry
