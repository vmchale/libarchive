module Codec.Archive.Pack.Lazy ( entriesToBSL
                               , entriesToBSL7zip
                               , entriesToBSLzip
                               ) where

import           Codec.Archive.Foreign
import           Codec.Archive.Monad
import           Codec.Archive.Pack
import           Codec.Archive.Types
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString        (packCStringLen)
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.DList             as DL
import           Data.Foldable          (toList)
import           Data.Functor           (($>))
import           Data.IORef             (modifyIORef', newIORef, readIORef)
import           Foreign.Marshal.Alloc  (free, mallocBytes)
import           Foreign.Ptr
import           System.IO.Unsafe       (unsafePerformIO)

-- | @since 1.0.5.0
entriesToBSLzip :: Foldable t => t Entry -> BSL.ByteString
entriesToBSLzip = unsafePerformIO . noFail . entriesToBSLGeneral archive_write_set_format_zip
{-# NOINLINE entriesToBSLzip #-}

-- | @since 1.0.5.0
entriesToBSL7zip :: Foldable t => t Entry -> BSL.ByteString
entriesToBSL7zip = unsafePerformIO . noFail . entriesToBSLGeneral archive_write_set_format_7zip
{-# NOINLINE entriesToBSL7zip #-}

-- | In general, this will be more efficient than 'entriesToBS'
--
-- @since 1.0.5.0
entriesToBSL :: Foldable t => t Entry -> BSL.ByteString
entriesToBSL = unsafePerformIO . noFail . entriesToBSLGeneral archive_write_set_format_pax_restricted
{-# NOINLINE entriesToBSL #-}

entriesToBSLGeneral :: Foldable t => (Ptr Archive -> IO ArchiveError) -> t Entry -> ArchiveM BSL.ByteString
entriesToBSLGeneral modifier hsEntries' = do
    a <- liftIO archive_write_new
    bsRef <- liftIO $ newIORef mempty
    oc <- liftIO $ mkOpenCallback doNothing
    wc <- liftIO $ mkWriteCallback (writeBSL bsRef)
    cc <- liftIO $ mkCloseCallback (\_ ptr -> freeHaskellFunPtr oc *> freeHaskellFunPtr wc *> free ptr $> ArchiveOk)
    nothingPtr <- liftIO $ mallocBytes 0
    ignore $ modifier a
    handle $ archiveWriteOpen a nothingPtr oc wc cc
    packEntries a hsEntries'
    ignore $ archive_free a
    BSL.fromChunks . toList <$> liftIO (readIORef bsRef)

    where writeBSL bsRef _ _ bufPtr sz = do
            let bytesRead = min sz (32 * 1024)
            bsl <- packCStringLen (bufPtr, fromIntegral bytesRead)
            modifyIORef' bsRef (`DL.snoc` bsl)
            pure bytesRead
          doNothing _ _ = pure ArchiveOk
