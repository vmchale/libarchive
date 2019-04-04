module Codec.Archive.Monad ( handle
                           , ignore
                           , runArchiveM
                           -- * Bracketed resources within 'ArchiveM'
                           , withCStringArchiveM
                           , useAsCStringLenArchiveM
                           , ArchiveM
                           ) where

import           Codec.Archive.Types
import           Control.Monad          (void)
import           Control.Monad.Except   (ExceptT, runExceptT, throwError)
import           Control.Monad.IO.Class
import           Data.ByteString        (useAsCStringLen)
import qualified Data.ByteString        as BS
import           Foreign.C.String

type ArchiveM = ExceptT ArchiveResult IO

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

flipArchiveM :: IO (Either a b) -> ExceptT a IO b
flipArchiveM act = do
    res <- liftIO act
    case res of
        Right x -> pure x
        Left y  -> throwError y

withCStringArchiveM :: String -> (CString -> ExceptT a IO b) -> ExceptT a IO b
withCStringArchiveM str f = flipArchiveM $ withCString str $ runExceptT . f

useAsCStringLenArchiveM :: BS.ByteString -> (CStringLen -> ExceptT a IO b) -> ExceptT a IO b
useAsCStringLenArchiveM bs f = flipArchiveM $ useAsCStringLen bs $ runExceptT . f
