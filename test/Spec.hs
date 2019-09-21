module Main ( main ) where

import           Codec.Archive
import qualified Data.ByteString.Lazy as BSL
import           Data.Either          (isRight)
import           Data.Foldable        (traverse_)
import           System.Directory     (listDirectory)
import           System.FilePath      ((</>))
import           Test.Hspec

roundtrip :: FilePath -> IO (Either ArchiveResult BSL.ByteString)
roundtrip = fmap (fmap entriesToBSL . readArchiveBSL) . BSL.readFile

testFp :: FilePath -> Spec
testFp fp = parallel $ it ("sucessfully packs/unpacks itself (" ++ fp ++ ")") $
    roundtrip fp >>= (`shouldSatisfy` isRight)

main :: IO ()
main = do
    tarballs <- listDirectory "test/data"
    hspec $
        describe "roundtrip" $ traverse_ testFp
            (("test/data" </>) <$> tarballs)
