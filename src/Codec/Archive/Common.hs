module Codec.Archive.Common ( actFree ) where

import           Codec.Archive.Foreign
import           Foreign.Ptr

-- | Read from an 'Archive' and then free it
actFree :: (Ptr Archive -> IO a) -> Ptr Archive -> IO a
actFree fact a = fact a <* archive_free a
