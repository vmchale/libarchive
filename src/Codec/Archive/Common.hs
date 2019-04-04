module Codec.Archive.Common ( actFree
                            , actFreeCallback
                            ) where

import           Codec.Archive.Foreign
import           Foreign.Ptr

-- | Do something with an 'Archive' and then free it
actFree :: (Ptr Archive -> IO a) -> Ptr Archive -> IO a
actFree fact a = fact a <* archive_free a

actFreeCallback :: (Ptr Archive -> IO a)
                -> (Ptr Archive, IO ()) -- ^ 'Ptr' to an 'Archive' and an 'IO' action to clean up when done
                -> IO a
actFreeCallback fact (a, freeAct) = fact a <* archive_free a <* freeAct
