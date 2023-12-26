module Codec.Archive.Internal.Monad ( handle
                                    , ignore
                                    , lenient
                                    , runArchiveM
                                    , throwArchiveM
                                    -- * Bracketed resources within 'ArchiveM'
                                    , withCStringArchiveM
                                    , useAsCStringLenArchiveM
                                    , allocaBytesArchiveM
                                    , ArchiveM
                                    ) where

import           Codec.Archive.Types
import           Control.Exception      (throw)
import           Control.Monad          (void)
import           Control.Monad.Except   (ExceptT, runExceptT, throwError)
import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import           Foreign.C.String
import           Foreign.Marshal.Alloc  (allocaBytes)
import           Foreign.Ptr            (Ptr)

type ArchiveM = ExceptT ArchiveResult IO

-- for things we don't think is going to fail
ignore :: IO ArchiveResult -> ArchiveM ()
ignore = void . liftIO

-- | Throws 'ArchiveResult' on error.
--
-- @since 2.2.5.0
throwArchiveM :: ArchiveM a -> IO a
throwArchiveM = fmap (either throw id) . runArchiveM

runArchiveM :: ArchiveM a -> IO (Either ArchiveResult a)
runArchiveM = runExceptT

-- TODO: ArchiveFailed Writer monad?
-- archive_clear_error
lenient :: IO ArchiveResult -> ArchiveM ()
lenient act = do
    res <- liftIO act
    case res of
        ArchiveFatal -> throw res
        ArchiveEOF   -> throw res
        _            -> pure ()

handle :: IO ArchiveResult -> ArchiveM ()
handle act = do
    res <- liftIO act
    case res of
        ArchiveOk    -> pure ()
        ArchiveRetry -> pure ()
        -- FIXME: ArchiveFailed may be ok
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

allocaBytesArchiveM :: Int -> (Ptr a -> ExceptT b IO c) -> ExceptT b IO c
allocaBytesArchiveM = genBracket allocaBytes

withCStringArchiveM :: String -> (CString -> ExceptT a IO b) -> ExceptT a IO b
withCStringArchiveM = genBracket withCString

useAsCStringLenArchiveM :: BS.ByteString -> (CStringLen -> ExceptT a IO b) -> ExceptT a IO b
useAsCStringLenArchiveM = genBracket BS.unsafeUseAsCStringLen
