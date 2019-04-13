module Main (main) where

import           Codec.Archive
import           Codec.Archive.Tar      (Entries (..), FormatError)
import qualified Codec.Archive.Tar      as Tar
import           Control.Monad.IO.Class (liftIO)
import           Criterion.Main
import qualified Data.ByteString.Lazy   as BSL
import           System.IO.Temp         (withSystemTempDirectory)

roundtrip :: BSL.ByteString -> Either ArchiveResult BSL.ByteString
roundtrip = fmap entriesToBSL . readArchiveBSL

failTar :: Entries a -> Either a [Tar.Entry]
failTar (Next e es) = (e :) <$> failTar es
failTar Done        = Right []
failTar (Fail e)    = Left e

roundtripTar :: BSL.ByteString -> Either FormatError BSL.ByteString
roundtripTar = fmap Tar.write . failTar . Tar.read

unpack :: IO (Either ArchiveResult ())
unpack = withSystemTempDirectory "libarchive" $
    \fp -> runArchiveM $ unpackToDirLazy fp =<< liftIO (BSL.readFile "libarchive-0.2.1.2.tar")

extractTar :: IO ()
extractTar = withSystemTempDirectory "tar" $
    \fp -> Tar.extract fp "libarchive-0.2.1.2.tar"

main :: IO ()
main =
    defaultMain [ env file $ \ f ->
                  bgroup "roundtrip"
                      [ bench "libarchive" $ nf roundtrip f
                      , bench "tar" $ nf roundtripTar f
                      ]
                , bgroup "unpack"
                      [ bench "libarchive" $ nfIO unpack
                      , bench "tar" $ nfIO extractTar
                      ]
                ]
    where file = BSL.readFile "libarchive-0.2.1.2.tar"
