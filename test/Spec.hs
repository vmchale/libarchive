module Main ( main ) where

import           Codec.Archive
import qualified Data.ByteString.Lazy as BSL
import           Data.Either          (isRight)
import           Test.Hspec

-- TODO: unpack llvm?
-- FIXME entriesToBSL is fucked

roundtrip :: IO (Either ArchiveResult BSL.ByteString)
roundtrip = do
    f <- BSL.readFile "libarchive-0.2.1.2.tar"
    case readArchiveBSL f of
        Right x -> Right <$> entriesToBSL x
        Left y  -> pure (Left y)

main :: IO ()
main = hspec $
    describe "roundtrip" $ parallel $
        it "Sucessfully packs/unpacks itself" $
            roundtrip >>= (`shouldSatisfy` isRight)
