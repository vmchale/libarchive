module Main (main) where

import           Codec.Archive
import           Codec.Archive.Tar    (Entries (..), FormatError)
import qualified Codec.Archive.Tar    as Tar
import           Criterion.Main
import qualified Data.ByteString.Lazy as BSL

roundtrip :: BSL.ByteString -> Either ArchiveResult BSL.ByteString
roundtrip = fmap entriesToBSL . readArchiveBSL

failTar :: Entries a -> Either a [Tar.Entry]
failTar (Next e es) = (e :) <$> failTar es
failTar Done        = Right []
failTar (Fail e)    = Left e

roundtripTar :: BSL.ByteString -> Either FormatError BSL.ByteString
roundtripTar = fmap Tar.write . failTar . Tar.read

main :: IO ()
main =
    defaultMain [ env file $ \ f ->
                  bgroup "roundtrip"
                      [ bench "libarchive" $ nf roundtrip f
                      , bench "tar" $ nf roundtripTar f
                      ]
                ]
    where file = BSL.readFile "libarchive-0.2.1.2.tar"
