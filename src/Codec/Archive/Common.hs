module Codec.Archive.Common ( actFree
                            , actFreeCallback
                            ) where

import           Codec.Archive.Foreign
import           Control.Monad.IO.Class (MonadIO (..))
import           Foreign.Ptr

-- | Do something with an 'Archive' and then free it
actFree :: MonadIO m
        => (Ptr Archive -> m a)
        -> Ptr Archive
        -> m a
actFree fact a = fact a <* liftIO (archive_free a)

actFreeCallback :: MonadIO m
                => (Ptr Archive -> m a)
                -> (Ptr Archive, IO ()) -- ^ 'Ptr' to an 'Archive' and an 'IO' action to clean up when done
                -> m a
actFreeCallback fact (a, freeAct) = fact a <* liftIO (archive_free a) <* liftIO freeAct
