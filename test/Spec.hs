module Main ( main ) where

import           Codec.Archive
import qualified Data.ByteString.Lazy as BSL
import           Data.Either          (isRight)
import           Test.Hspec

-- TODO: unpack llvm?
-- FIXME entriesToBSL is fucked

roundtrip :: IO (Either ArchiveResult BSL.ByteString)
roundtrip = fmap entriesToBSL . readArchiveBSL <$> BSL.readFile "test/data/libarchive-0.2.1.2.tar"

main :: IO ()
main = hspec $
    describe "roundtrip" $ parallel $
        it "Sucessfully packs/unpacks itself" $
            roundtrip >>= (`shouldSatisfy` isRight)
