module Main ( main ) where

import           Codec.Archive
import           Control.Exception    (throw)
import qualified Data.ByteString.Lazy as BSL

forceList :: [a] -> IO ()
forceList = (`seq` mempty) . last

main :: IO ()
main =
    sequence_
        [ readArc
        , readWriteArc
        , readArcBS
        ]

readArc :: IO ()
readArc = forceList =<< throwArchiveM
    (readArchiveFile "test/data/llvm-9.0.0.src.tar")

readArcBS :: IO ()
readArcBS = (forceList . either throw id) . readArchiveBSL =<< BSL.readFile "test/data/llvm-9.0.0.src.tar"

readWriteArc :: IO ()
readWriteArc = forceList . BSL.toChunks =<< throwArchiveM
    (entriesToBSL <$> readArchiveFile "test/data/llvm-9.0.0.src.tar")
