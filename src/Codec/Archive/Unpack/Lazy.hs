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
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Ptr
import           Foreign.Storable      (poke)
import           System.IO.Unsafe      (unsafePerformIO)

unpackToDirLazy :: FilePath -- ^ Directory to unpack in
                -> BSL.ByteString -- ^ 'BSL.ByteString' containing archive
                -> IO ()
unpackToDirLazy fp bs = do
    a <- bslToArchive bs
    unpackEntriesFp a fp
    void $ archive_read_free a

-- | Read an archive lazily. The format of the archive is automatically
-- detected.
readArchiveBSL :: BSL.ByteString -> [Entry]
readArchiveBSL = unsafePerformIO . (actFree hsEntries <=< bslToArchive)

-- | Lazily stream a 'BSL.ByteString'
bslToArchive :: BSL.ByteString -> IO (Ptr Archive)
bslToArchive bs = do
    a <- archive_read_new
    void $ archive_read_support_format_all a
    bsChunksRef <- newIORef bsChunks
    rc <- mkReadCallback (readBSL bsChunksRef)
    cc <- mkCloseCallback (\_ ptr -> freeHaskellFunPtr rc *> free ptr $> archiveOk)
    nothingPtr <- mallocBytes 0
    sequence_ [ archive_read_set_read_callback a rc
              , archive_read_set_close_callback a cc
              , archive_read_set_callback_data a nothingPtr
              -- Sooo readBSL bsChunksRef a nothingPtr nothingPtr works BUT when
              -- I use the wrapper it's all fucked??
              --
              -- I should try to pin down a minimal case...
              --
              -- this is where it hangs indefinitely
              , archive_read_open1 a
              ]
    pure a
    where readBSL bsRef _ _ dataPtr = do
                -- when readBSL is called it's fucked??
                bs' <- readIORef bsRef
                case bs' of
                    [] -> pure 0
                    (x:_) -> do
                        modifyIORef bsRef tail
                        useAsCStringLen x $ \(charPtr, sz) ->
                            poke dataPtr charPtr $>
                            fromIntegral sz
          bsChunks = BSL.toChunks bs
