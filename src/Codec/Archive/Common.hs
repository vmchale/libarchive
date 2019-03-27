module Codec.Archive.Common ( actFree
                            , actFreeCallback
                            ) where

import           Codec.Archive.Foreign
import           Foreign.Marshal.Alloc (free)
import           Foreign.Ptr

-- | Read from an 'Archive' and then free it
actFree :: (Ptr Archive -> IO a) -> Ptr Archive -> IO a
actFree fact a = fact a <* archive_free a

actFreeCallback :: (Ptr Archive -> IO a) -> (Ptr Archive, IO ()) -> IO a
actFreeCallback fact (a, freeAct) = fact a <* archive_free a <* freeAct
