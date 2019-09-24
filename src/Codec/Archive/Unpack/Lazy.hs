module Codec.Archive.Unpack.Lazy ( readArchiveBSL
                                 , unpackToDirLazy
                                 ) where

import           Codec.Archive.Common
import           Codec.Archive.Foreign
import           Codec.Archive.Monad
import           Codec.Archive.Types
import           Codec.Archive.Unpack
import           Control.Composition    ((.**))
import           Control.Monad          (void, (<=<))
import           Control.Monad.IO.Class
import           Data.ByteString        (useAsCStringLen)
import qualified Data.ByteString.Lazy   as BSL
import           Data.Foldable          (traverse_)
import           Data.Functor           (($>))
import           Data.IORef             (modifyIORef, newIORef, readIORef,
                                         writeIORef)
import           Foreign.C.Types
import           Foreign.Marshal.Alloc  (free, mallocBytes, reallocBytes)
import           Foreign.Ptr
import           Foreign.Storable       (poke)
import           System.IO.Unsafe       (unsafeDupablePerformIO)

foreign import ccall memcpy :: Ptr a -- ^ Destination
                            -> Ptr b -- ^ Source
                            -> CSize -- ^ Size
                            -> IO (Ptr a) -- ^ Pointer to destination

hmemcpy :: Ptr a -> Ptr b -> CSize -> IO ()
hmemcpy = void .** memcpy

-- | In general, this will be more efficient than 'unpackToDir'
--
-- The 'BSL.ByteString' chunks should be @32k@ bytes.
--
-- @since 1.0.4.0
unpackToDirLazy :: FilePath -- ^ Directory to unpack in
                -> BSL.ByteString -- ^ 'BSL.ByteString' containing archive
                -> ArchiveM ()
unpackToDirLazy fp bs = do
    (a, act) <- bslToArchive bs
    unpackEntriesFp a fp
    ignore $ archiveFree a
    liftIO act

-- | Read an archive lazily. The format of the archive is automatically
-- detected.
--
-- In general, this will be more efficient than 'readArchiveBS'
--
-- The 'BSL.ByteString' chunks should be @32k@ bytes.
--
-- @since 1.0.4.0
readArchiveBSL :: BSL.ByteString -> Either ArchiveResult [Entry]
readArchiveBSL = unsafeDupablePerformIO . runArchiveM . (actFreeCallback hsEntries <=< bslToArchive)
{-# NOINLINE readArchiveBSL #-}

-- | Lazily stream a 'BSL.ByteString'
bslToArchive :: BSL.ByteString
             -> ArchiveM (Ptr Archive, IO ()) -- ^ Returns an 'IO' action to be used to clean up after we're done with the archive
bslToArchive bs = do
    a <- liftIO archiveReadNew
    ignore $ archiveReadSupportFormatAll a
    bufPtr <- liftIO $ mallocBytes (32 * 1024) -- default to 32k byte chunks
    bsChunksRef <- liftIO $ newIORef bsChunks
    bufSzRef <- liftIO $ newIORef (32 * 1024)
    rc <- liftIO $ mkReadCallback (readBSL bsChunksRef bufSzRef bufPtr)
    cc <- liftIO $ mkCloseCallback (\_ ptr -> freeHaskellFunPtr rc *> free ptr $> ArchiveOk)
    nothingPtr <- liftIO $ mallocBytes 0
    let seqErr = traverse_ handle
    seqErr [ archiveReadSetReadCallback a rc
           , archiveReadSetCloseCallback a cc
           , archiveReadSetCallbackData a nothingPtr
           , archiveReadOpen1 a
           ]
    pure (a, freeHaskellFunPtr cc *> free bufPtr)

    where readBSL bsRef bufSzRef bufPtr _ _ dataPtr = do
                bs' <- readIORef bsRef
                case bs' of
                    [] -> pure 0
                    (x:_) -> do
                        modifyIORef bsRef tail
                        useAsCStringLen x $ \(charPtr, sz) -> do
                            bufSz <- readIORef bufSzRef
                            bufPtr' <- if sz > bufSz
                                then writeIORef bufSzRef sz *> reallocBytes bufPtr sz
                                else pure bufPtr
                            hmemcpy bufPtr' charPtr (fromIntegral sz)
                            poke dataPtr bufPtr' $> fromIntegral sz
          bsChunks = BSL.toChunks bs
