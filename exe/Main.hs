{-# LANGUAGE RecordWildCards #-}

module Main where

import           Codec.Archive                 hiding (unpackToDir)
import           Control.Monad                 (forM_)
import           Control.Monad.Catch           (Exception, MonadThrow, throwM)
import           Data.Char                     (toUpper)
import           Data.Bits                     (testBit)
import           Data.List                     (isSuffixOf)
import           System.IO                     (stderr, hPutStrLn)
import           System.Directory              (getCurrentDirectory, setCurrentDirectory)
import           System.Console.GetOpt         (OptDescr(..), ArgDescr(..), ArgOrder(..),
                                               getOpt', usageInfo)
import           System.Environment            (getArgs)
import           System.Exit                   (exitFailure)
import           Data.Time                     (formatTime)
import           Data.Time.Clock.POSIX         (posixSecondsToUTCTime, POSIXTime)
import           Data.Time                     (defaultTimeLocale)

import qualified Codec.Compression.BZip        as BZip
import qualified Codec.Compression.GZip        as GZip
import qualified Codec.Compression.Lzma        as Lzma
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString               as BS


throwEither :: (Exception a, MonadThrow m) => Either a b -> m b
throwEither a = case a of
  Left  e -> throwM e
  Right r -> pure r

throwEitherM :: (Exception a, MonadThrow m) => m (Either a b) -> m b
throwEitherM a = a >>= throwEither


data Compression = None | GZip | BZip | XZ
  deriving (Show, Eq)

compressionFromFileName :: FilePath -> Compression
compressionFromFileName fn
  | ".tar.gz"  `isSuffixOf` fn = GZip
  | ".tar.xz"  `isSuffixOf` fn = XZ
  | ".tar.bz2" `isSuffixOf` fn = BZip
  | otherwise                  = None

compress :: Compression -> BL.ByteString -> BL.ByteString
compress GZip = GZip.compress
compress BZip = BZip.compress
compress XZ   = Lzma.compress
compress None = id

decompress :: Compression -> BL.ByteString -> BL.ByteString
decompress GZip = GZip.decompress
decompress BZip = BZip.decompress
decompress XZ   = Lzma.decompress
decompress None = id

data Verbosity = Verbose | Concise

main' :: Options -> [FilePath] -> IO ()
main' (Options { optFile        = file,
                 optDir         = dir',
                 optAction      = action,
                 optCompression = compression',
                 optVerbosity   = verbosity }) files =
  case action of
    NoAction -> die ["No action given. Specify one of -c, -t or -x."]
    Help     -> printUsage
    Create   -> do
      dir <- getDir
      setCurrentDirectory dir
      bs <- packFiles files
      writeOutput . compress compression $ bs
    Extract  -> do
      dir <- getDir
      input <- getInput
      throwEitherM . runArchiveM . unpackToDirLazy dir . decompress compression $ input
    List     -> do
      input <- getInput
      entries <- throwEither . readArchiveBSL . decompress compression $ input
      printEntries entries
    Append -> die ["Not implemented yet!"]
  where
    getInput    = if file == "-" then BL.getContents else BL.readFile  file
    writeOutput = if file == "-" then BL.putStr      else BL.writeFile file
    getDir      = if dir' == ""  then getCurrentDirectory else pure dir'
    compression = case compression' of
      None -> compressionFromFileName file
      c    -> c
    printEntries entries = forM_ entries printEntry
    printEntry = putStrLn . entryInfo verbosity


main :: IO ()
main = do
  (opts, files) <- parseOptions =<< getArgs
  main' opts files


------------------------
-- List archive contents

entryInfo :: Verbosity -> Entry FilePath BS.ByteString -> String
entryInfo Verbose = detailedInfo
entryInfo Concise = filepath

detailedInfo :: Entry FilePath BS.ByteString -> String
detailedInfo Entry{..} =
  unwords [ typeCode : permissions'
          , justify 19 (owner ++ '/' : group) size
          , time'
          , name ++ link ]
  where
    typeCode = case content of
      Hardlink _  -> 'h'
      Symlink _ _ -> 'l'
      Directory   -> 'd'
      _           -> '-'
    permissions' = concat [userPerms, groupPerms, otherPerms]
      where
        userPerms  = formatPerms 8 7 6 11 's'
        groupPerms = formatPerms 5 4 3 10 's'
        otherPerms = formatPerms 2 1 0  9 't'
        formatPerms r w x s c =
          [if testBit m r then 'r' else '-'
          ,if testBit m w then 'w' else '-'
          ,if testBit m s
             then if testBit m x then c   else toUpper c
             else if testBit m x then 'x' else '-']
        m = permissions
    owner = nameOrID ownerName ownerId
    group = nameOrID groupName groupId
    (Ownership ownerName groupName ownerId groupId) = ownership
    nameOrID Nothing i  = show i
    nameOrID (Just n) _ = n
    size = case content of
             NormalFile c -> show (BS.length c)
             _            -> "0"

    time' = maybe "unknown" (formatEpochTime "%Y-%m-%d %H:%M") time
    name = filepath
    link = case content of
      Hardlink     l   -> " link to " ++ l
      Symlink l _ -> " -> "      ++ l
      _                -> ""

justify :: Int -> String -> String -> String
justify width left right = left ++ padding ++ right
  where
    padding  = replicate padWidth ' '
    padWidth = max 1 (width - length left - length right)

formatEpochTime :: String -> ModTime -> String
formatEpochTime f (t, _) =
    formatTime defaultTimeLocale f . posixSecondsToUTCTime $ (realToFrac t :: POSIXTime)


------------------------
-- Command line handling (stolen from 'tar')

data Options = Options {
    optFile        :: FilePath, -- "-" means stdin/stdout
    optDir         :: FilePath,
    optAction      :: Action,
    optCompression :: Compression,
    optVerbosity   :: Verbosity
  }

defaultOptions :: Options
defaultOptions = Options {
    optFile        = "-",
    optDir         = "",
    optAction      = NoAction,
    optCompression = None,
    optVerbosity   = Concise
  }

data Action = NoAction
            | Help
            | Create
            | Extract
            | List
            | Append
  deriving Show

optDescr :: [OptDescr (Options -> Options)]
optDescr =
  [ Option ['c'] ["create"]
      (action Create)
      "Create a new archive."
  , Option ['x'] ["extract", "get"]
      (action Extract)
      "Extract files from an archive."
  , Option ['t'] ["list"]
      (action List)
      "List the contents of an archive."
  , Option ['r'] ["append"]
      (action Append)
      "Append files to the end of an archive."
  , Option ['f'] ["file"]
      (ReqArg (\f o -> o { optFile = f}) "ARCHIVE")
      "Use archive file ARCHIVE."
  , Option ['C'] ["directory"]
      (ReqArg (\d o -> o { optDir = d }) "DIR")
      "Create or extract relative to DIR."
  , Option ['z'] ["gzip", "gunzip", "ungzip"]
      (compression GZip)
      "Use gzip compression."
  , Option ['j'] ["bzip2"]
      (compression BZip)
      "Use bzip2 compression."
  , Option ['J'] ["xz"]
      (compression XZ)
      "Use xz compression."
  , Option ['v'] ["verbose"]
      (NoArg (\o -> o { optVerbosity = Verbose }))
      "Verbosely list files processed."
  , Option ['h', '?'] ["help"]
      (action Help)
      "Print this help output."
  ]
  where
    action      a = NoArg (\o -> o { optAction = a })
    compression c = NoArg (\o -> o { optCompression = c })

printUsage :: IO ()
printUsage = putStrLn (usageInfo headder optDescr)
  where
    headder = unlines ["archive creates and extracts TAR archives.",
                       "",
                       "Usage: archive [OPTION ...] [FILE ...]"]

parseOptions :: [String] -> IO (Options, [FilePath])
parseOptions args =
  let (fs, files, nonopts, errors) = getOpt' Permute optDescr args
  in case (nonopts, errors) of
       ([], [])    -> return $ (foldl (flip ($)) defaultOptions fs, files)
       (_ , (_:_)) -> die errors
       (_ ,  _)    -> die (map (("unrecognized option "++).show) nonopts)


die :: [String] -> IO a
die errs = do
  mapM_ (\e -> hPutStrLn stderr $ "archive: " ++ e) $ errs
  hPutStrLn stderr "Try `archive --help' for more information."
  exitFailure
