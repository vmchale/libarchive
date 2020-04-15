module Main ( main ) where

import           Codec.Archive
import qualified Data.ByteString.Lazy as BSL

forceList :: [a] -> IO ()
forceList = (`seq` mempty) . last

main :: IO ()
main = readArc *> readWriteArc

readArc :: IO ()
readArc = forceList =<< throwArchiveM
    (readArchiveFile "test/data/llvm-9.0.0.src.tar")

readWriteArc :: IO ()
readWriteArc = forceList . BSL.toChunks =<< throwArchiveM
    (entriesToBSL <$> readArchiveFile "test/data/llvm-9.0.0.src.tar")
