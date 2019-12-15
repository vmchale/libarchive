{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import           Codec.Archive
import           Codec.Archive.Roundtrip (itPacksUnpacks, itPacksUnpacksViaFS, roundtrip, roundtripFreaky, roundtripSmall)
import           Codec.Archive.Test
import           Data.Either             (isRight)
import           Data.Foldable           (traverse_)
import           System.Directory        (doesDirectoryExist, listDirectory)
import           System.FilePath         ((</>))
import           Test.Hspec

testFp :: HasCallStack => FilePath -> Spec
testFp fp = parallel $ it ("sucessfully unpacks/packs (" ++ fp ++ ")") $
    roundtrip fp >>= (`shouldSatisfy` isRight)

testFpFreaky :: FilePath -> Spec
testFpFreaky fp = parallel $ it ("works on nonstandard bytestring (" ++ fp ++ ")") $
    roundtripFreaky fp >>= (`shouldSatisfy` isRight)

testFpSmall :: FilePath -> Spec
testFpSmall fp = parallel $ it ("works on small bytestring (" ++ fp ++ ")") $
    roundtripSmall fp >>= (`shouldSatisfy` isRight)

main :: IO ()
main = do

    dir <- doesDirectoryExist "test/data"
    tarballs <- if dir then listDirectory "test/data" else pure []
    let tarPaths = ("test/data" </>) <$> tarballs

    hspec $
        describe "roundtrip" $ do

            traverse_ testFp tarPaths
            traverse_ testFpFreaky tarPaths
            traverse_ testFpSmall tarPaths

            context "with symlinks" $ do
                let entries =
                        [ simpleDir "x/"
                        , simpleFile "x/a.txt" (NormalFile "referenced")
                        , simpleFile "x/b.txt" (Symlink "a.txt")
                        ]
                itPacksUnpacks entries
                itPacksUnpacksViaFS entries

            context "with hardlinks" $ do
                let entries =
                        [ simpleDir "x/"
                        , simpleFile "x/a.txt" (NormalFile "shared")
                        , simpleFile "x/b.txt" (Hardlink "x/a.txt")
                        ]
                itPacksUnpacks entries
                context "issue#4" $ itPacksUnpacksViaFS entries

            context "with forward referenced hardlinks" $ do
                let entries =
                        [ simpleDir "x/"
                        , simpleFile "x/b.txt" (Hardlink "x/a.txt")
                        , simpleFile "x/a.txt" (NormalFile "shared")
                        ]
                itPacksUnpacks entries
                xcontext "re-ordering on unpack" $ itPacksUnpacksViaFS entries

            xcontext "having entry without ownership" . itPacksUnpacks $
                [ stripOwnership (simpleFile "a.txt" (NormalFile "text")) ]
            xcontext "having entry without timestamp" . itPacksUnpacks $
                [ stripTime (simpleFile "a.txt" (NormalFile "text")) ]
