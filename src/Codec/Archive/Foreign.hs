module Codec.Archive.Foreign ( -- * cbits
                               unpack_in_dir
                             , unpack_from_file
                             -- * Direct bindings
                             , archive_read_new
                             , archive_read_support_format_all
                             , archive_entry_set_pathname
                             , archive_read_data_skip
                             ) where

import           Codec.Archive.Types
import           Data.Word           (Word)
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr

foreign import ccall unsafe unpack_in_dir :: CString -> Ptr CChar -> Word -> IO ()
foreign import ccall unsafe unpack_from_file :: CString -> CString -> IO ()

foreign import ccall unsafe archive_read_new :: IO (Ptr Archive)
foreign import ccall unsafe archive_read_support_format_all :: Ptr Archive -> IO ()
foreign import ccall unsafe archive_entry_set_pathname :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall unsafe archive_read_data_skip :: Ptr Archive -> IO ()
