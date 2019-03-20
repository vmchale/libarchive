module Codec.Archive.Foreign.Common ( intToBool ) where

import           Foreign.C.Types

-- TODO: this should check against ArchiveFatal or whatnot?
intToBool :: CInt -> Bool
intToBool = toEnum . fromIntegral
