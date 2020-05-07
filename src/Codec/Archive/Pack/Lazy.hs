module Codec.Archive.Pack.Lazy ( entriesToBSL
                               , entriesToBSL7zip
                               , entriesToBSLzip
                               , entriesToBSLCpio
                               , entriesToBSLXar
                               , packFiles
                               , packFilesZip
                               , packFiles7zip
                               , packFilesCpio
                               , packFilesXar
                               ) where

import           Codec.Archive.Foreign
import           Codec.Archive.Monad
import           Codec.Archive.Pack
import           Codec.Archive.Pack.Common
import           Codec.Archive.Types
import           Control.Composition       ((.@))
import           Control.Monad.IO.Class    (liftIO)
import           Data.ByteString           (packCStringLen)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.DList                as DL
import           Data.Foldable             (toList)
import           Data.Functor              (($>))
import           Data.IORef                (modifyIORef', newIORef, readIORef)
import           Foreign.Concurrent        (newForeignPtr)
import           Foreign.ForeignPtr        (castForeignPtr, finalizeForeignPtr)
import           Foreign.Marshal.Alloc     (free, mallocBytes)
import           Foreign.Ptr               (castPtr, freeHaskellFunPtr)
import           System.IO.Unsafe          (unsafeDupablePerformIO)

packer :: (Traversable t) => (t Entry -> BSL.ByteString) -> t FilePath -> IO BSL.ByteString
packer = traverse mkEntry .@ fmap

-- | Pack files into a tar archive. This will be more efficient than
--
-- @BSL.writeFile fp . entriesToBSL@
--
-- @since 2.0.0.0
packFiles :: Traversable t
          => t FilePath -- ^ Filepaths relative to the current directory
          -> IO BSL.ByteString
packFiles = packer entriesToBSL

-- | @since 2.0.0.0
packFilesZip :: Traversable t => t FilePath -> IO BSL.ByteString
packFilesZip = packer entriesToBSLzip

-- | @since 2.0.0.0
packFiles7zip :: Traversable t => t FilePath -> IO BSL.ByteString
packFiles7zip = packer entriesToBSL7zip

-- | @since 2.2.3.0
packFilesCpio :: Traversable t => t FilePath -> IO BSL.ByteString
packFilesCpio = packer entriesToBSLCpio

-- | @since 2.2.4.0
packFilesXar :: Traversable t => t FilePath -> IO BSL.ByteString
packFilesXar = packer entriesToBSLXar

-- | @since 1.0.5.0
entriesToBSLzip :: Foldable t => t Entry -> BSL.ByteString
entriesToBSLzip = unsafeDupablePerformIO . noFail . entriesToBSLGeneral archiveWriteSetFormatZip
{-# NOINLINE entriesToBSLzip #-}

-- | @since 1.0.5.0
entriesToBSL7zip :: Foldable t => t Entry -> BSL.ByteString
entriesToBSL7zip = unsafeDupablePerformIO . noFail . entriesToBSLGeneral archiveWriteSetFormat7zip
{-# NOINLINE entriesToBSL7zip #-}

-- | @since 2.2.3.0
entriesToBSLCpio :: Foldable t => t Entry -> BSL.ByteString
entriesToBSLCpio = unsafeDupablePerformIO . noFail . entriesToBSLGeneral archiveWriteSetFormatCpio
{-# NOINLINE entriesToBSLCpio #-}

-- | @since 2.2.4.0
entriesToBSLXar :: Foldable t => t Entry -> BSL.ByteString
entriesToBSLXar = unsafeDupablePerformIO . noFail . entriesToBSLGeneral archiveWriteSetFormatXar
{-# NOINLINE entriesToBSLXar #-}

-- | In general, this will be more efficient than 'entriesToBS'
--
-- @since 1.0.5.0
entriesToBSL :: Foldable t => t Entry -> BSL.ByteString
entriesToBSL = unsafeDupablePerformIO . noFail . entriesToBSLGeneral archiveWriteSetFormatPaxRestricted
{-# NOINLINE entriesToBSL #-}

entriesToIOChunks :: Foldable t
                  => (ArchivePtr -> IO ArchiveResult) -- ^ Action to set format of archive
                  -> t Entry
                  -> (BS.ByteString -> IO ()) -- ^ 'IO' Action to process the chunks
                  -> ArchiveM ()
entriesToIOChunks modifier hsEntries' chunkAct = do
    preA <- liftIO archiveWriteNew
    oc <- liftIO $ mkOpenCallback doNothing
    wc <- liftIO $ mkWriteCallback chunkHelper
    -- signal "done" in the finalizer??
    cc <- liftIO $ mkCloseCallback (\_ ptr -> freeHaskellFunPtr oc *> freeHaskellFunPtr wc *> free ptr $> ArchiveOk)
    a <- liftIO $ castForeignPtr <$> newForeignPtr (castPtr preA) (archiveFree preA *> freeHaskellFunPtr cc)
    nothingPtr <- liftIO $ mallocBytes 0
    ignore $ modifier a
    handle $ archiveWriteOpen a nothingPtr oc wc cc
    packEntries a hsEntries'
    liftIO $ finalizeForeignPtr a

    where doNothing _ _ = pure ArchiveOk
          chunkHelper _ _ bufPtr sz = do
            let bytesRead = min (fromIntegral sz) (32 * 1024)
            bs <- packCStringLen (bufPtr, fromIntegral bytesRead)
            chunkAct bs
            pure bytesRead

entriesToBSLGeneral :: Foldable t => (ArchivePtr -> IO ArchiveResult) -> t Entry -> ArchiveM BSL.ByteString
entriesToBSLGeneral modifier hsEntries' = do
    preRef <- liftIO $ newIORef mempty
    let chunkAct = writeBSL preRef
    entriesToIOChunks modifier hsEntries' chunkAct
    BSL.fromChunks . toList <$> liftIO (readIORef preRef)

    where writeBSL bsRef chunk =
            modifyIORef' bsRef (`DL.snoc` chunk)
