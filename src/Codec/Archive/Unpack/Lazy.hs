module Codec.Archive.Unpack.Lazy ( readArchiveBSL
                                 , bslToArchive
                                 , unpackToDirLazy
                                 ) where

import           Codec.Archive.Common
import           Codec.Archive.Foreign
import           Codec.Archive.Types
import           Codec.Archive.Unpack
import           Control.Monad         (void, (<=<))
import           Data.ByteString       (useAsCStringLen)
import qualified Data.ByteString.Lazy  as BSL
import           Data.Functor          (($>))
import           Data.IORef            (modifyIORef, newIORef, readIORef)
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Ptr
import           Foreign.Storable      (poke)
import           System.IO.Unsafe      (unsafePerformIO)

foreign import ccall memcpy :: Ptr a -- ^ Destination
                            -> Ptr b -- ^ Source
                            -> CSize -- ^ Size
                            -> IO (Ptr a) -- ^ Pointer to destination

unpackToDirLazy :: FilePath -- ^ Directory to unpack in
                -> BSL.ByteString -- ^ 'BSL.ByteString' containing archive
                -> IO ()
unpackToDirLazy fp bs = do
    (a, act) <- bslToArchive bs
    unpackEntriesFp a fp
    void $ archive_read_free a
    act

-- | Read an archive lazily. The format of the archive is automatically
-- detected.
readArchiveBSL :: BSL.ByteString -> [Entry]
readArchiveBSL = unsafePerformIO . (actFreeCallback hsEntries <=< bslToArchive)

-- | Lazily stream a 'BSL.ByteString'
bslToArchive :: BSL.ByteString
             -> IO (Ptr Archive, IO ()) -- ^ Returns an 'IO' action to be used to clean up after we're done with the archive
bslToArchive bs = do
    a <- archive_read_new
    void $ archive_read_support_format_all a
    bufPtr <- mallocBytes (32 * 1024) -- default to 32k byte chunks; should really do something more rigorous
    bsChunksRef <- newIORef bsChunks
    rc <- mkReadCallback (readBSL bsChunksRef bufPtr)
    cc <- mkCloseCallback (\_ ptr -> freeHaskellFunPtr rc *> free ptr $> archiveOk)
    nothingPtr <- mallocBytes 0
    sequence_ [ archive_read_set_read_callback a rc
              , archive_read_set_close_callback a cc
              , archive_read_set_callback_data a nothingPtr
              , archive_read_open1 a
              ]
    pure (a, freeHaskellFunPtr cc *> free bufPtr)

    where readBSL bsRef bufPtr _ _ dataPtr = do
                bs' <- readIORef bsRef
                case bs' of
                    [] -> pure 0
                    (x:_) -> do
                        modifyIORef bsRef tail
                        useAsCStringLen x $ \(charPtr, sz) -> do
                            void $ memcpy bufPtr charPtr (fromIntegral sz)
                            poke dataPtr bufPtr $> fromIntegral sz
          bsChunks = BSL.toChunks bs
