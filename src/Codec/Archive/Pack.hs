module Codec.Archive.Pack ( entriesToFile
                          , entriesToFileZip
                          , entriesToFile7Zip
                          , entriesToBS
                          , entriesToBSzip
                          , entriesToBS7zip
                          , packEntries
                          , noFail
                          , packToFile
                          , packToFileZip
                          , packToFile7Zip
                          ) where

import           Codec.Archive.Foreign
import           Codec.Archive.Monad
import           Codec.Archive.Pack.Common
import           Codec.Archive.Types
import           Control.Monad             (void)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Data.ByteString           (packCStringLen)
import qualified Data.ByteString           as BS
import           Data.Foldable             (sequenceA_, traverse_)
import           Data.Semigroup            (Sum (..))
import           Foreign.C.String
import           Foreign.Ptr               (Ptr)
import           Foreign.Storable          (peek)
import           System.IO.Unsafe          (unsafePerformIO)

maybeDo :: Applicative f => Maybe (f ()) -> f ()
maybeDo = sequenceA_

contentAdd :: EntryContent -> Ptr Archive -> Ptr ArchiveEntry -> ArchiveM ()
contentAdd (NormalFile contents) a entry = do
    liftIO $ archive_entry_set_filetype entry regular
    liftIO $ archive_entry_set_size entry (fromIntegral (BS.length contents))
    handle $ archiveWriteHeader a entry
    useAsCStringLenArchiveM contents $ \(buff, sz) ->
        liftIO $ void $ archive_write_data a buff (fromIntegral sz)
contentAdd Directory a entry = do
    liftIO $ archive_entry_set_filetype entry directory
    handle $ archiveWriteHeader a entry
contentAdd (Symlink fp) a entry = do
    liftIO $ archive_entry_set_filetype entry symlink
    liftIO $ withCString fp $ \fpc ->
        archive_entry_set_symlink entry fpc
    handle $ archiveWriteHeader a entry

withMaybeCString :: Maybe String -> (Maybe CString -> IO a) -> IO a
withMaybeCString (Just x) f = withCString x (f . Just)
withMaybeCString Nothing f  = f Nothing

setOwnership :: Ownership -> Ptr ArchiveEntry -> IO ()
setOwnership (Ownership uname gname uid gid) entry =
    withMaybeCString uname $ \unameC ->
    withMaybeCString gname $ \gnameC ->
    traverse_ maybeDo
        [ archive_entry_set_uname entry <$> unameC
        , archive_entry_set_gname entry <$> gnameC
        , Just (archive_entry_set_uid entry (fromIntegral uid))
        , Just (archive_entry_set_gid entry (fromIntegral gid))
        ]

setTime :: ModTime -> Ptr ArchiveEntry -> IO ()
setTime (time', nsec) entry = archive_entry_set_mtime entry time' nsec

packEntries :: (Foldable t) => Ptr Archive -> t Entry -> ArchiveM ()
packEntries a = traverse_ (archiveEntryAdd a)

-- Get a number of bytes appropriate for creating the archive.
entriesSz :: (Foldable t, Integral a) => t Entry -> a
entriesSz = getSum . foldMap (Sum . entrySz)
    where entrySz e = 512 + 512 * (contentSz (content e) `div` 512 + 1)
          contentSz (NormalFile str) = fromIntegral $ BS.length str
          contentSz Directory        = 0
          contentSz (Symlink fp)     = fromIntegral $ length fp

-- | Returns a 'BS.ByteString' containing a tar archive with the 'Entry's
--
-- @since 1.0.0.0
entriesToBS :: Foldable t => t Entry -> BS.ByteString
entriesToBS = unsafePerformIO . noFail . entriesToBSGeneral archiveWriteSetFormatPaxRestricted
{-# NOINLINE entriesToBS #-}

-- | Returns a 'BS.ByteString' containing a @.7z@ archive with the 'Entry's
--
-- @since 1.0.0.0
entriesToBS7zip :: Foldable t => t Entry -> BS.ByteString
entriesToBS7zip = unsafePerformIO . noFail . entriesToBSGeneral archiveWriteSetFormat7Zip
{-# NOINLINE entriesToBS7zip #-}

-- | Returns a 'BS.ByteString' containing a zip archive with the 'Entry's
--
-- @since 1.0.0.0
entriesToBSzip :: Foldable t => t Entry -> BS.ByteString
entriesToBSzip = unsafePerformIO . noFail . entriesToBSGeneral archiveWriteSetFormatZip
{-# NOINLINE entriesToBSzip #-}

-- This is for things we don't think will fail. When making a 'BS.ByteString'
-- from a bunch of 'Entry's, for instance, we don't anticipate any errors
noFail :: ArchiveM a -> IO a
noFail act = do
    res <- runArchiveM act
    case res of
        Right x -> pure x
        Left _  -> error "Should not fail."

-- | Internal function to be used with 'archive_write_set_format_pax' etc.
entriesToBSGeneral :: (Foldable t) => (Ptr Archive -> IO ArchiveResult) -> t Entry -> ArchiveM BS.ByteString
entriesToBSGeneral modifier hsEntries' = do
    a <- liftIO archive_write_new
    ignore $ modifier a
    allocaArchiveM $ \used ->
        allocaBytesArchiveM bufSize $ \buffer -> do
            handle $ archiveWriteOpenMemory a buffer bufSize used
            packEntries a hsEntries'
            handle $ archiveWriteClose a
            usedSz <- liftIO $ peek used
            res <- liftIO $ curry packCStringLen buffer (fromIntegral usedSz)
            ignore $ archiveFree a
            pure res

    where bufSize :: Integral a => a
          bufSize = entriesSz hsEntries'

filePacker :: (Traversable t) => (FilePath -> t Entry -> ArchiveM ()) -> FilePath -> t FilePath -> ArchiveM ()
filePacker f tar fps = f tar =<< liftIO (traverse mkEntry fps)

-- | @since 1.1.0.0
packToFile :: Traversable t
           => FilePath -- ^ @.tar@ archive to be created
           -> t FilePath -- ^ Files to include
           -> ArchiveM ()
packToFile = filePacker entriesToFile

-- | @since 1.1.0.0
packToFileZip :: Traversable t
              => FilePath
              -> t FilePath
              -> ArchiveM ()
packToFileZip = filePacker entriesToFileZip

-- | @since 1.1.0.0
packToFile7Zip :: Traversable t
               => FilePath
               -> t FilePath
               -> ArchiveM ()
packToFile7Zip = filePacker entriesToFile7Zip

-- | Write some entries to a file, creating a tar archive. This is more
-- efficient than
--
-- @
-- BS.writeFile "file.tar" (entriesToBS entries)
-- @
--
-- @since 1.0.0.0
entriesToFile :: Foldable t => FilePath -> t Entry -> ArchiveM ()
entriesToFile = entriesToFileGeneral archiveWriteSetFormatPaxRestricted
-- this is the recommended format; it is a tar archive

-- | Write some entries to a file, creating a zip archive.
--
-- @since 1.0.0.0
entriesToFileZip :: Foldable t => FilePath -> t Entry -> ArchiveM ()
entriesToFileZip = entriesToFileGeneral archiveWriteSetFormatZip

-- | Write some entries to a file, creating a @.7z@ archive.
--
-- @since 1.0.0.0
entriesToFile7Zip :: Foldable t => FilePath -> t Entry -> ArchiveM ()
entriesToFile7Zip = entriesToFileGeneral archiveWriteSetFormat7Zip

entriesToFileGeneral :: Foldable t => (Ptr Archive -> IO ArchiveResult) -> FilePath -> t Entry -> ArchiveM ()
entriesToFileGeneral modifier fp hsEntries' = do
    a <- liftIO archive_write_new
    ignore $ modifier a
    withCStringArchiveM fp $ \fpc ->
        handle $ archiveWriteOpenFilename a fpc
    packEntries a hsEntries'
    ignore $ archiveFree a

withArchiveEntry :: MonadIO m => (Ptr ArchiveEntry -> m a) -> m a
withArchiveEntry fact = do
    entry <- liftIO archive_entry_new
    res <- fact entry
    liftIO $ archive_entry_free entry
    pure res

archiveEntryAdd :: Ptr Archive -> Entry -> ArchiveM ()
archiveEntryAdd a (Entry fp contents perms owner mtime) =
    withArchiveEntry $ \entry -> do
        liftIO $ withCString fp $ \fpc ->
            archive_entry_set_pathname entry fpc
        liftIO $ archive_entry_set_perm entry perms
        liftIO $ setOwnership owner entry
        liftIO $ maybeDo (setTime <$> mtime <*> pure entry)
        contentAdd contents a entry
