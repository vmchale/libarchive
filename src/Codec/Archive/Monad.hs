module Codec.Archive.Monad ( handle
                           , ignore
                           , runArchiveM
                           -- * Bracketed resources within 'ArchiveM'
                           , withCStringArchiveM
                           , useAsCStringLenArchiveM
                           , allocaArchiveM
                           , allocaBytesArchiveM
                           , ArchiveM
                           ) where

import           Codec.Archive.Types
import           Control.Monad          (void)
import           Control.Monad.Except   (ExceptT, runExceptT, throwError)
import           Control.Monad.IO.Class
import           Data.ByteString        (useAsCStringLen)
import qualified Data.ByteString        as BS
import           Foreign.C.String
import           Foreign.Marshal.Alloc  (alloca, allocaBytes)
import           Foreign.Ptr            (Ptr)
import           Foreign.Storable       (Storable)

type ArchiveM = ExceptT ArchiveResult IO

-- TODO: ArchiveM a -> IO a where we throw exceptions using the error string?

-- for stuff we think isn't going to fail
ignore :: IO ArchiveError -> ArchiveM ()
ignore = void . liftIO

runArchiveM :: ArchiveM a -> IO (Either ArchiveResult a)
runArchiveM = runExceptT

handle :: IO ArchiveResult -> ArchiveM ()
handle act = do
    res <- liftIO act
    case res of
        ArchiveOk    -> pure ()
        ArchiveRetry -> pure ()
        x            -> throwError x

flipExceptIO :: IO (Either a b) -> ExceptT a IO b
flipExceptIO act = do
    res <- liftIO act
    case res of
        Right x -> pure x
        Left y  -> throwError y

genBracket :: (a -> (b -> IO (Either c d)) -> IO (Either c d)) -- ^ Function like 'withCString' we are trying to lift
           -> a -- ^ Fed to @b@
           -> (b -> ExceptT c IO d) -- ^ The action
           -> ExceptT c IO d
genBracket f x = flipExceptIO . f x . (runExceptT .)

allocaArchiveM :: Storable a => (Ptr a -> ExceptT b IO c) -> ExceptT b IO c
allocaArchiveM = genBracket (pure alloca) ()

allocaBytesArchiveM :: Int -> (Ptr a -> ExceptT b IO c) -> ExceptT b IO c
allocaBytesArchiveM = genBracket allocaBytes

withCStringArchiveM :: String -> (CString -> ExceptT a IO b) -> ExceptT a IO b
withCStringArchiveM = genBracket withCString

useAsCStringLenArchiveM :: BS.ByteString -> (CStringLen -> ExceptT a IO b) -> ExceptT a IO b
useAsCStringLenArchiveM = genBracket useAsCStringLen
