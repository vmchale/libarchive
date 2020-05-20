module Codec.Archive.Unpack ( hsEntries
                            , unpackEntriesFp
                            , unpackArchive
                            , readArchiveFile
                            , readArchiveBS
                            , unpackToDir
                            ) where

import           Codec.Archive.Common
import           Codec.Archive.Foreign
import           Codec.Archive.Monad
import           Codec.Archive.Types
import           Control.Monad                ((<=<))
import           Control.Monad.IO.Class       (liftIO)
import qualified Control.Monad.ST.Lazy        as LazyST
import qualified Control.Monad.ST.Lazy.Unsafe as LazyST
import           Data.Bifunctor               (first)
import qualified Data.ByteString              as BS
import           Data.Functor                 (void, ($>))
import           Foreign.C.String
import           Foreign.Concurrent           (newForeignPtr)
import           Foreign.ForeignPtr           (castForeignPtr, newForeignPtr_)
import           Foreign.Marshal.Alloc        (allocaBytes, free, mallocBytes)
import           Foreign.Ptr                  (castPtr, nullPtr)
import           System.FilePath              ((</>))
import           System.IO.Unsafe             (unsafeDupablePerformIO)

-- | Read an archive contained in a 'BS.ByteString'. The format of the archive is
-- automatically detected.
--
-- @since 1.0.0.0
readArchiveBS :: BS.ByteString -> Either ArchiveResult [Entry]
readArchiveBS = unsafeDupablePerformIO . runArchiveM . (actFreeCallback hsEntries <=< bsToArchive)
{-# NOINLINE readArchiveBS #-}

bsToArchive :: BS.ByteString -> ArchiveM (ArchivePtr, IO ())
bsToArchive bs = do
    preA <- liftIO archiveReadNew
    a <- liftIO $ castForeignPtr <$> newForeignPtr (castPtr preA) (void $ archiveFree preA)
    ignore $ archiveReadSupportFormatAll a
    bufPtr <- useAsCStringLenArchiveM bs $
        \(buf, sz) -> do
            buf' <- liftIO $ mallocBytes sz
            _ <- liftIO $ hmemcpy buf' buf (fromIntegral sz)
            handle $ archiveReadOpenMemory a buf (fromIntegral sz)
            pure buf'
    pure (a, free bufPtr)

-- | Read an archive from a file. The format of the archive is automatically
-- detected.
--
-- @since 1.0.0.0
readArchiveFile :: FilePath -> ArchiveM [Entry]
readArchiveFile fp = act =<< liftIO (do
    pre <- archiveReadNew
    castForeignPtr <$> newForeignPtr (castPtr pre) (void $ archiveFree pre))

    where act a =
            archiveFile fp a $> LazyST.runST (hsEntriesST a)

archiveFile :: FilePath -> ArchivePtr -> ArchiveM ()
archiveFile fp a = withCStringArchiveM fp $ \cpath ->
    ignore (archiveReadSupportFormatAll a) *>
    handle (archiveReadOpenFilename a cpath 10240)

-- | This is more efficient than
--
-- @
-- unpackToDir "llvm" =<< BS.readFile "llvm.tar"
-- @
unpackArchive :: FilePath -- ^ Filepath pointing to archive
              -> FilePath -- ^ Dirctory to unpack in
              -> ArchiveM ()
unpackArchive tarFp dirFp = do
    preA <- liftIO archiveReadNew
    a <- liftIO $ castForeignPtr <$> newForeignPtr (castPtr preA) (void $ archiveFree preA)
    act a

    where act a =
            archiveFile tarFp a *>
            unpackEntriesFp a dirFp

readEntry :: ArchivePtr -> ArchiveEntryPtr -> IO Entry
readEntry a entry =
    Entry
        <$> (peekCString =<< archiveEntryPathname entry)
        <*> readContents a entry
        <*> archiveEntryPerm entry
        <*> readOwnership entry
        <*> readTimes entry

-- | Yield the next entry in an archive
getHsEntry :: ArchivePtr -> IO (Maybe Entry)
getHsEntry a = do
    entry <- getEntry a
    case entry of
        Nothing -> pure Nothing
        Just x  -> Just <$> readEntry a x

-- | Return a list of 'Entry's.
hsEntries :: ArchivePtr -> ArchiveM [Entry]
hsEntries p = pure (LazyST.runST $ hsEntriesST p)

-- | Return a list of 'Entry's.
hsEntriesST :: ArchivePtr -> LazyST.ST s [Entry]
hsEntriesST a = do
    next <- LazyST.unsafeIOToST (getHsEntry a)
    case next of
        Nothing -> pure []
        Just x  -> (x:) <$> hsEntriesST a

-- | Unpack an archive in a given directory
unpackEntriesFp :: ArchivePtr -> FilePath -> ArchiveM ()
unpackEntriesFp a fp = do
    res <- liftIO $ getEntry a
    case res of
        Nothing -> pure ()
        Just x  -> do
            preFile <- liftIO $ archiveEntryPathname x
            file <- liftIO $ peekCString preFile
            let file' = fp </> file
            liftIO $ withCString file' $ \fileC ->
                archiveEntrySetPathname x fileC
            ft <- liftIO $ archiveEntryFiletype x
            case ft of
                Just{} ->
                    ignore $ archiveReadExtract a x archiveExtractTime
                Nothing -> do
                    hardlink <- liftIO $ peekCString =<< archiveEntryHardlink x
                    let hardlink' = fp </> hardlink
                    liftIO $ withCString hardlink' $ \hl ->
                        archiveEntrySetHardlink x hl
                    ignore $ archiveReadExtract a x archiveExtractTime
            ignore $ archiveReadDataSkip a
            unpackEntriesFp a fp

readBS :: ArchivePtr -> Int -> IO BS.ByteString
readBS a sz =
    allocaBytes sz $ \buff ->
        archiveReadData a buff (fromIntegral sz) *>
        BS.packCStringLen (buff, sz)

readContents :: ArchivePtr -> ArchiveEntryPtr -> IO EntryContent
readContents a entry = go =<< archiveEntryFiletype entry
    where go Nothing            = Hardlink <$> (peekCString =<< archiveEntryHardlink entry)
          go (Just FtRegular)   = NormalFile <$> (readBS a =<< sz)
          go (Just FtLink)      = Symlink <$> (peekCString =<< archiveEntrySymlink entry) <*> archiveEntrySymlinkType entry
          go (Just FtDirectory) = pure Directory
          go (Just _)           = error "Unsupported filetype"
          sz = fromIntegral <$> archiveEntrySize entry

archiveGetterHelper :: (ArchiveEntryPtr -> IO a) -> (ArchiveEntryPtr -> IO Bool) -> ArchiveEntryPtr -> IO (Maybe a)
archiveGetterHelper get check entry = do
    check' <- check entry
    if check'
        then Just <$> get entry
        else pure Nothing

archiveGetterNull :: (ArchiveEntryPtr -> IO CString) -> ArchiveEntryPtr -> IO (Maybe String)
archiveGetterNull get entry = do
    res <- get entry
    if res == nullPtr
        then pure Nothing
        else fmap Just (peekCString res)

readOwnership :: ArchiveEntryPtr -> IO Ownership
readOwnership entry =
    Ownership
        <$> archiveGetterNull archiveEntryUname entry
        <*> archiveGetterNull archiveEntryGname entry
        <*> (fromIntegral <$> archiveEntryUid entry)
        <*> (fromIntegral <$> archiveEntryGid entry)

readTimes :: ArchiveEntryPtr -> IO (Maybe ModTime)
readTimes = archiveGetterHelper go archiveEntryMtimeIsSet
    where go entry =
            (,) <$> archiveEntryMtime entry <*> archiveEntryMtimeNsec entry

-- | Get the next 'ArchiveEntry' in an 'Archive'
getEntry :: ArchivePtr -> IO (Maybe ArchiveEntryPtr)
getEntry a = do
    let done ArchiveOk    = False
        done ArchiveRetry = False
        done _            = True
    (stop, res) <- first done <$> archiveReadNextHeader a
    if stop
        then pure Nothing
        else Just . castForeignPtr <$> newForeignPtr_ (castPtr res)

unpackToDir :: FilePath -- ^ Directory to unpack in
            -> BS.ByteString -- ^ 'BS.ByteString' containing archive
            -> ArchiveM ()
unpackToDir fp bs = do
    (a, act) <- bsToArchive bs
    unpackEntriesFp a fp
    liftIO act
