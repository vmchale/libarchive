module Codec.Archive.Foreign ( unpackToDir
                             ) where
import           Data.ByteString  as BS
import           Data.Word        (Word)
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr
import           System.FilePath  (pathSeparator)

foreign import ccall unsafe unpack_in_dir :: CString -> Ptr CChar -> Word -> IO ()

unpackToDir :: FilePath
            -> BS.ByteString
            -> IO ()
unpackToDir fp bs = do
    fp' <- newCString (fp ++ [pathSeparator])
    useAsCStringLen bs $
        \(charPtr, sz) ->
            unpack_in_dir fp' charPtr (fromIntegral sz)
