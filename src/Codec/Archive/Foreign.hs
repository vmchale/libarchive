module Codec.Archive.Foreign ( unpack_in_dir
                             , unpack_from_file
                             ) where

import           Data.Word        (Word)
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr

foreign import ccall unsafe unpack_in_dir :: CString -> Ptr CChar -> Word -> IO ()
foreign import ccall unsafe unpack_from_file :: CString -> CString -> IO ()
