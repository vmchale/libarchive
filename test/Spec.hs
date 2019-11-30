{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main ( main ) where

import           Codec.Archive
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Either          (isRight)
import           Data.Foldable        (traverse_)
import           System.Directory     (doesDirectoryExist, listDirectory)
import           System.FilePath      ((</>))
import           Data.List            (intersperse)
import           Test.Hspec


newtype TestEntries = TestEntries [Entry]
    deriving (Eq)

instance Show (TestEntries) where
    showsPrec _ (TestEntries entries) = ("(TestEntries [" ++) . joinBy (", "++) (map showsEntry entries) . ("])" ++) where
        showsEntry entry = ("Entry " ++) .
            ("{filepath=" ++) . shows (filepath entry) .
            (", content=" ++) . showsContent (content entry) .
            (", permissions=" ++) . shows (permissions entry) .
            (", ownership=" ++) . shows (ownership entry) .
            (", time=" ++) . shows (time entry) .
            ("}" ++)
        showsContent = \case
            NormalFile bytes -> ("(NormalFile $ " ++) . shows (BS.take 10 bytes) . (" <> undefined)" ++)
            Directory -> ("Directory" ++)
            Symlink target -> ("(Symlink " ++) . shows target . (')':)
            Hardlink target -> ("(Hardlink " ++) . shows target . (')':)
        joinBy :: ShowS -> [ShowS] -> ShowS
        joinBy sep = foldr (.) id . intersperse sep

roundtrip :: FilePath -> IO (Either ArchiveResult BSL.ByteString)
roundtrip = fmap (fmap entriesToBSL . readArchiveBSL) . BSL.readFile

itPacksUnpacks :: HasCallStack => [Entry] -> Spec
itPacksUnpacks entries = it "packs/unpacks successfully without loss" $ do
        (TestEntries <$> unpacked) `shouldBe` (Right $ TestEntries entries)
    where
        packed = entriesToBSL entries
        unpacked = readArchiveBSL packed

testFp :: HasCallStack => FilePath -> Spec
testFp fp = parallel $ it ("sucessfully unpacks/packs (" ++ fp ++ ")") $
    roundtrip fp >>= (`shouldSatisfy` isRight)

main :: IO ()
main = do
    dir <- doesDirectoryExist "test/data"
    tarballs <- if dir then listDirectory "test/data" else pure []
    hspec $
        describe "roundtrip" $ do
            traverse_ testFp (("test/data" </>) <$> tarballs)
            context "with symlinks" . itPacksUnpacks $
                [ simpleFile "a.txt" (NormalFile "referenced")
                , simpleFile "b.txt" (Symlink "a.txt")
                ]
            context "with hardlinks" . itPacksUnpacks $
                [ simpleFile "a.txt" (NormalFile "shared")
                , simpleFile "b.txt" (Hardlink "a.txt")
                ]
            context "withforward referenced hardlinks" . itPacksUnpacks $
                [ simpleFile "b.txt" (Hardlink "a.txt")
                , simpleFile "a.txt" (NormalFile "shared")
                ]
            xcontext "having entry without ownership" . itPacksUnpacks $
                [ stripOwnership (simpleFile "a.txt" (NormalFile "text")) ]
            xcontext "having entry without timestamp" . itPacksUnpacks $
                [ stripTime (simpleFile "a.txt" (NormalFile "text")) ]

simpleFile :: FilePath -> EntryContent -> Entry
simpleFile name what = Entry name what standardPermissions (Ownership (Just "root") (Just "root")  0 0) (Just (0,0))

stripOwnership, stripTime :: Entry -> Entry
stripOwnership entry = entry { ownership = Ownership Nothing Nothing 0 0 }
stripTime entry = entry { time = Nothing }
