module Codec.Archive
    ( unpackToDir
    , unpackArchive
    ) where

import           Codec.Archive.Foreign
import           Data.ByteString       (useAsCStringLen)
import qualified Data.ByteString       as BS
import           Foreign.C.String
import           System.FilePath       (pathSeparator)

unpackArchive :: FilePath -- ^ Filepath pointing to archive
              -> FilePath -- ^ Filepath to unpack to
              -> IO ()
unpackArchive tarFp dirFp = do
    fp' <- newCString tarFp
    dir' <- newCString (dirFp ++ [pathSeparator])
    unpack_from_file dir' fp'

unpackToDir :: FilePath -- ^ Directory to unpack in
            -> BS.ByteString -- ^ 'ByteString' containing archive
            -> IO ()
unpackToDir fp bs = do
    fp' <- newCString (fp ++ [pathSeparator])
    useAsCStringLen bs $
        \(charPtr, sz) ->
            unpack_in_dir fp' charPtr (fromIntegral sz)
