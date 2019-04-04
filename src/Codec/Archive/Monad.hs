module Codec.Archive.Monad ( handle
                           , runArchiveM
                           , ArchiveM
                           ) where

import           Codec.Archive.Types
import           Control.Monad.Except   (ExceptT, runExceptT, throwError)
import           Control.Monad.IO.Class

type ArchiveM = ExceptT ArchiveResult IO

runArchiveM :: ArchiveM a -> IO (Either ArchiveResult a)
runArchiveM = runExceptT

handle :: (a -> IO ArchiveResult) -> a -> ArchiveM ()
handle act dat = do
    res <- liftIO $ act dat
    case res of
        ArchiveOk    -> pure ()
        ArchiveRetry -> pure ()
        x            -> throwError x

