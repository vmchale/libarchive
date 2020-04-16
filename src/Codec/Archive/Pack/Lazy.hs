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
import qualified Control.Monad.ST.Lazy     as LazyST
import           Data.ByteString           (packCStringLen)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.DList                as DL
import           Data.Foldable             (toList)
import           Data.Functor              (($>))
import           Data.IORef                (IORef, modifyIORef', newIORef, readIORef)
import           Foreign.Concurrent        (newForeignPtr)
import           Foreign.ForeignPtr        (castForeignPtr, finalizeForeignPtr)
import           Foreign.Marshal.Alloc     (free, mallocBytes)
import           Foreign.Ptr               (castPtr, freeHaskellFunPtr)
import           System.IO.Unsafe          (unsafeDupablePerformIO, unsafeInterleaveIO)

packer :: (Traversable t) => (t Entry -> BSL.ByteString) -> t FilePath -> IO BSL.ByteString
packer = traverse mkEntry .@ fmap

-- | Pack files into a tar archive
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
entriesToBSLzip es = LazyST.runST (noFailST $ entriesToBSLGeneral archiveWriteSetFormatZip es)
{-# NOINLINE entriesToBSLzip #-}

-- | @since 1.0.5.0
entriesToBSL7zip :: Foldable t => t Entry -> BSL.ByteString
entriesToBSL7zip es = LazyST.runST (noFailST $ entriesToBSLGeneral archiveWriteSetFormat7zip es)
{-# NOINLINE entriesToBSL7zip #-}

-- | @since 2.2.3.0
entriesToBSLCpio :: Foldable t => t Entry -> BSL.ByteString
entriesToBSLCpio es = LazyST.runST (noFailST $ entriesToBSLGeneral archiveWriteSetFormatCpio es)
{-# NOINLINE entriesToBSLCpio #-}

-- | @since 2.2.4.0
entriesToBSLXar :: Foldable t => t Entry -> BSL.ByteString
entriesToBSLXar es = LazyST.runST (noFailST $ entriesToBSLGeneral archiveWriteSetFormatXar es)
{-# NOINLINE entriesToBSLXar #-}

-- | In general, this will be more efficient than 'entriesToBS'
--
-- @since 1.0.5.0
entriesToBSL :: Foldable t => t Entry -> BSL.ByteString
entriesToBSL es = LazyST.runST (noFailST $ entriesToBSLGeneral archiveWriteSetFormatPaxRestricted es)
{-# NOINLINE entriesToBSL #-}

entriesToBSLGeneral :: Foldable t => (ArchivePtr -> IO ArchiveResult) -> t Entry -> ArchiveST s BSL.ByteString
entriesToBSLGeneral modifier hsEntries' = do
    (a, bsRef) <- setup
    packEntriesST a hsEntries'
    unsafeArchiveMToArchiveST $ do
        liftIO $ finalizeForeignPtr a
        BSL.fromChunks . toList <$> liftIO (unsafeInterleaveIO $ readIORef bsRef)


    where writeBSL bsRef _ _ bufPtr sz = do
            let bytesRead = min (fromIntegral sz) (32 * 1024)
            bsl <- packCStringLen (bufPtr, fromIntegral bytesRead)
            tok <- unsafeInterleaveIO $ modifyIORef' bsRef (`DL.snoc` bsl)
            pure (tok `seq` bytesRead)
          doNothing _ _ = pure ArchiveOk
          -- FIXME: this part isn't sufficiently lazy

          setup :: ArchiveST s (ArchivePtr, IORef (DL.DList BS.ByteString))
          setup = unsafeArchiveMToArchiveST $ do
            preA <- liftIO archiveWriteNew
            bsRef <- liftIO $ newIORef mempty
            oc <- liftIO $ mkOpenCallback doNothing
            wc <- liftIO $ mkWriteCallback (writeBSL bsRef)
            cc <- liftIO $ mkCloseCallback (\_ ptr -> freeHaskellFunPtr oc *> freeHaskellFunPtr wc *> free ptr $> ArchiveOk)
            a <- liftIO $ castForeignPtr <$> newForeignPtr (castPtr preA) (archiveFree preA *> freeHaskellFunPtr cc)
            nothingPtr <- liftIO $ mallocBytes 0
            ignore $ modifier a
            handle $ archiveWriteOpen a nothingPtr oc wc cc
            pure (a, bsRef)
