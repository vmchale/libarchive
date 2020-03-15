module Main ( main ) where

import           Codec.Archive
import           Control.Exception (throw)
import           System.IO.Unsafe  (unsafeInterleaveIO)

forceList :: [a] -> IO ()
forceList = (`seq` mempty) . last

main :: IO ()
main = readArc

readArc :: IO ()
readArc = either throw forceList =<< runArchiveM
    (readArchiveFile "test/data/llvm-9.0.0.src.tar")
