module Codec.Archive.Pack.Common ( mkEntry ) where

import           Codec.Archive.Types
import qualified Data.ByteString          as BS
import           System.PosixCompat.Files (fileGroup, fileMode, fileOwner,
                                           getFileStatus, isDirectory,
                                           isRegularFile, isSymbolicLink,
                                           readSymbolicLink)

mkContent :: FilePath -> IO EntryContent
mkContent fp = do
    status <- getFileStatus fp
    let res = (isRegularFile status, isDirectory status, isSymbolicLink status)
    case res of
        (True, False, False) -> NormalFile <$> BS.readFile fp
        (False, True, False) -> pure Directory
        (False, False, True) -> Symlink <$> readSymbolicLink fp
        (_, _, _)            -> error "inconsistent read result"

mkEntry :: FilePath -> IO Entry
mkEntry fp = do
    status <- getFileStatus fp
    content' <- mkContent fp
    pure $ Entry fp content' (fileMode status) (Ownership Nothing Nothing (fromIntegral $ fileOwner status) (fromIntegral $ fileGroup status)) Nothing

