module Codec.Archive.Foreign.Common ( intToBool ) where

import           Foreign.C.Types

intToBool :: CInt -> Bool
intToBool = toEnum . fromIntegral
