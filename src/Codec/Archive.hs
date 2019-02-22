module Codec.Archive
    ( unpackToDir
    , unpackArchive
    ) where

import           Codec.Archive.Foreign (unpackToDir)
import qualified Data.ByteString       as BS

unpackArchive :: FilePath -- ^ Filepath pointing to archive
              -> FilePath -- ^ Filepath to unpack to
              -> IO ()
unpackArchive tarFp dirFp = do
    contents <- BS.readFile tarFp
    unpackToDir dirFp contents
