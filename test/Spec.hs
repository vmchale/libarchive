{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import           Codec.Archive
import           Control.Composition        (thread)
import           Control.Monad.Except
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import           Data.Either                (isRight)
import           Data.Foldable              (traverse_)
import           Data.List                  (intersperse, sort)
import           System.Directory           (doesDirectoryExist, listDirectory, withCurrentDirectory)
import           System.Directory.Recursive (getDirRecursive)
import           System.FilePath            ((</>))
import           System.IO.Temp             (withSystemTempDirectory)
import           Test.Hspec


newtype TestEntries = TestEntries [Entry]
    deriving (Eq)

instance Show TestEntries where
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
        joinBy sep = thread . intersperse sep

roundtrip :: FilePath -> IO (Either ArchiveResult BSL.ByteString)
roundtrip = fmap (fmap entriesToBSL . readArchiveBSL) . BSL.readFile

itPacksUnpacks :: HasCallStack => [Entry] -> Spec
itPacksUnpacks entries = parallel $ it "packs/unpacks successfully without loss" $
        (TestEntries <$> unpacked) `shouldBe` Right (TestEntries entries)
    where
        packed = entriesToBSL entries
        unpacked = readArchiveBSL packed

itPacksUnpacksViaFS :: HasCallStack => [Entry] -> Spec
itPacksUnpacksViaFS entries = parallel $ unpackedFromFS $ it "packs/unpacks on filesystem successfully without loss" $ \unpacked ->
        fmap (fmap stripDotSlash . testEntries) unpacked `shouldBe` Right (testEntries entries)
    where
        -- Use this to test content as well
        -- testEntries = TestEntries . sortOn filepath . map (stripOwnership . stripPermissions)
        testEntries = sort . map filepath
        unpackedFromFS = around $ \action ->
            withSystemTempDirectory "spec-" $ \tmpdir -> do
            unpacked <- {- withCurrentDirectory tmpdir . -} runArchiveM $ do
                entriesToDir tmpdir entries
                packed <- liftIO . withCurrentDirectory tmpdir $ do
                    files <- getDirRecursive "."
                    packFiles files
                liftEither $ readArchiveBSL packed

            action unpacked
        stripDotSlash :: FilePath -> FilePath
        stripDotSlash ('.':'/':fp) = fp
        stripDotSlash fp           = fp

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

simpleFile :: FilePath -> EntryContent -> Entry
simpleFile name what = Entry name what standardPermissions (Ownership (Just "root") (Just "root")  0 0) (Just (0,0))

simpleDir :: FilePath -> Entry
simpleDir name = Entry name Directory dirPermissions (Ownership (Just "root") (Just "root")  0 0) (Just (0,0))

dirPermissions :: Permissions
dirPermissions = executablePermissions

-- TODO: expose something like this via archive_write_disk
-- entriesToDir :: Foldable t => FilePath -> t Entry -> ArchiveM ()
entriesToDir :: FilePath -> [Entry] -> ArchiveM ()
entriesToDir dest = unpackToDirLazy dest . entriesToBSL

stripOwnership, stripTime :: Entry -> Entry
stripOwnership entry = entry { ownership = Ownership Nothing Nothing 0 0 }
stripTime entry = entry { time = Nothing }
