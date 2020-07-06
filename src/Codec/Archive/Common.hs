module Codec.Archive.Common ( hmemcpy
                            ) where

import           Control.Composition ((.**))
import           Control.Monad       (void)
import           Foreign.C.Types     (CSize (..))
import           Foreign.Ptr

foreign import ccall memcpy :: Ptr a -- ^ Destination
                            -> Ptr b -- ^ Source
                            -> CSize -- ^ Size
                            -> IO (Ptr a) -- ^ Pointer to destination

hmemcpy :: Ptr a -> Ptr b -> CSize -> IO ()
hmemcpy = void .** memcpy
