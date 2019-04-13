module Codec.Archive.Foreign.Common ( intToBool ) where

import           Foreign.C.Types

-- TODO: this should check against ArchiveFatal?
intToBool :: CInt -> Bool
intToBool 0 = False
intToBool _ = True
