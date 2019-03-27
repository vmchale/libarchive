module Codec.Archive.Pack.Lazy ( entriesToBSL
                               , entriesToBSL7zip
                               , entriesToBSLzip
                               ) where

import           Codec.Archive.Foreign
import           Codec.Archive.Pack
import           Codec.Archive.Types
import           Control.Monad         (void)
import           Data.ByteString       (packCStringLen)
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.DList            as DL
import           Data.Foldable         (toList)
import           Data.Functor          (($>))
import           Data.IORef            (modifyIORef', newIORef, readIORef)
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Ptr
import           System.IO.Unsafe      (unsafePerformIO)

-- | @since 1.0.5.0
entriesToBSLzip :: Foldable t => t Entry -> BSL.ByteString
entriesToBSLzip = unsafePerformIO . entriesToBSLGeneral archive_write_set_format_zip
{-# NOINLINE entriesToBSLzip #-}

-- | @since 1.0.5.0
entriesToBSL7zip :: Foldable t => t Entry -> BSL.ByteString
entriesToBSL7zip = unsafePerformIO . entriesToBSLGeneral archive_write_set_format_7zip
{-# NOINLINE entriesToBSL7zip #-}

-- | In general, this will be more efficient than 'entriesToBS'
--
-- @since 1.0.5.0
entriesToBSL :: Foldable t => t Entry -> BSL.ByteString
entriesToBSL = unsafePerformIO . entriesToBSLGeneral archive_write_set_format_pax_restricted
{-# NOINLINE entriesToBSL #-}

-- I'm not sure if this actually streams anything or not but like...
entriesToBSLGeneral :: Foldable t => (Ptr Archive -> IO ArchiveError) -> t Entry -> IO BSL.ByteString
entriesToBSLGeneral modifier hsEntries' = do
    a <- archive_write_new
    bsRef <- newIORef mempty
    oc <- mkOpenCallback doNothing
    wc <- mkWriteCallback (writeBSL bsRef)
    cc <- mkCloseCallback (\_ ptr -> freeHaskellFunPtr oc *> freeHaskellFunPtr wc *> free ptr $> archiveOk)
    nothingPtr <- mallocBytes 0
    void $ modifier a
    void $ archive_write_open a nothingPtr oc wc cc
    packEntries a hsEntries'
    void $ archive_write_free a
    BSL.fromChunks . toList <$> readIORef bsRef

    where writeBSL bsRef _ _ bufPtr sz = do
            let bytesRead = min sz (32 * 1024)
            bsl <- packCStringLen (bufPtr, fromIntegral bytesRead)
            modifyIORef' bsRef (`DL.snoc` bsl)
            pure bytesRead
          doNothing _ _ = pure archiveOk
