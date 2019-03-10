-- | This module contains higher-level functions for working with archives in
-- Haskell. See "Codec.Archive.Foreign" for direct bindings to
-- libarchive.
module Codec.Archive
    ( -- * High-level functionality
      unpackToDir
    , unpackArchive
    , entriesToFile
    , hsEntries
    , readArchiveFile
    , standardPermissions
    , executablePermissions
    -- * Concrete (Haskell) types
    , Entry (..)
    , EntryContent (..)
    , Permissions
    -- * Abstract types
    , Archive
    , ArchiveEntry
    -- * Lower-level API types
    , ArchiveError
    , ExtractFlags
    , FileType
    , ArchiveFilter
    ) where

import           Codec.Archive.Foreign
import           Codec.Archive.Types
import           Control.Monad         (void, (<=<))
import           Data.ByteString       (useAsCStringLen)
import qualified Data.ByteString       as BS
import           Data.Foldable         (traverse_)
import           Foreign.C.String
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr           (Ptr)
import           Foreign.Storable      (Storable (..))
import           System.FilePath       ((</>))

-- TODO: memArchive and createFileArchive

withArchiveEntry :: (Ptr ArchiveEntry -> IO a) -> IO a
withArchiveEntry fact = do
    entry <- archive_entry_new
    res <- fact entry
    archive_entry_free entry
    pure res

contentAdd :: EntryContent -> Ptr Archive -> Ptr ArchiveEntry -> IO ()
contentAdd (NormalFile contents) a entry = do
    archive_entry_set_filetype entry regular
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

archiveEntryAdd :: Ptr Archive -> Entry -> IO ()
archiveEntryAdd a (Entry fp contents perms owner mtime) =
    withArchiveEntry $ \entry -> do
        withCString fp $ \fpc ->
            archive_entry_set_pathname entry fpc
        archive_entry_set_perm entry perms
        setOwnership owner entry
        setTime mtime entry
        contentAdd contents a entry
        pure ()

packEntries :: (Foldable t) => Ptr Archive -> t Entry -> IO ()
packEntries a = traverse_ (archiveEntryAdd a)

entriesToFile :: Foldable t => FilePath -> t Entry -> IO ()
entriesToFile fp hsEntries' = do
    a <- archive_write_new
    void $ archive_write_set_format_pax_restricted a
    withCString fp $ \fpc ->
        void $ archive_write_open_filename a fpc
    packEntries a hsEntries'
    void $ archive_write_free a

readArchiveFile :: FilePath -> IO [Entry]
readArchiveFile = hsEntries <=< archiveFile

archiveFile :: FilePath -> IO (Ptr Archive)
archiveFile fp = withCString fp $ \cpath -> do
    a <- archive_read_new
    void $ archive_read_support_format_all a
    void $ archive_read_open_filename a cpath 10240
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
            void $ archive_read_extract a x archiveExtractTime
            archive_entry_set_pathname x preFile
            void $ archive_read_data_skip a
            unpackEntriesFp a fp

readBS :: Ptr Archive -> IO BS.ByteString
readBS a =
    alloca $ \buff ->
    alloca $ \sz ->
    alloca $ \offset -> do
        void $ archive_read_data_block a buff sz offset
        cstr <- peek buff
        strSz <- peek sz
        BS.packCStringLen (cstr, fromIntegral strSz)

readContents :: Ptr Archive -> Ptr ArchiveEntry -> IO EntryContent
readContents a entry = go =<< archive_entry_filetype entry
    where go ft@(FileType n) | ft == regular = NormalFile <$> readBS a
                | ft == symlink = Symlink <$> (peekCString =<< archive_entry_symlink entry)
                | ft == directory = pure Directory
                | otherwise = error ("Unsupported filetype " ++ show n)

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

readEntry :: Ptr Archive -> Ptr ArchiveEntry -> IO Entry
readEntry a entry = do
    fp <- peekCString =<< archive_entry_pathname entry
    perms <- archive_entry_perm entry
    contents <- readContents a entry
    owner <- readOwnership entry
    times <- readTimes entry
    pure $ Entry fp contents perms owner times

hsEntries :: Ptr Archive -> IO [Entry]
hsEntries a = do
    next <- getHsEntry a
    case next of
        Nothing -> pure []
        Just x  -> (x:) <$> hsEntries a

getHsEntry :: Ptr Archive -> IO (Maybe Entry)
getHsEntry a = do
    entry <- getEntry a
    case entry of
        Nothing -> pure Nothing
        Just x  -> Just <$> readEntry a x

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
            -> BS.ByteString -- ^ 'ByteString' containing archive
            -> IO ()
unpackToDir fp bs = do
    a <- bsToArchive bs
    unpackEntriesFp a fp
    void $ archive_read_free a
