module Main (main) where

import           Control.Monad (when)
import           Distribution.C2Hs (c2hsBuildHooks, c2hsHaddockHooks, c2hsReplHooks)
import           Distribution.Simple (UserHooks(..), autoconfUserHooks, buildHook, defaultMainWithHooks, simpleUserHooks)
import           Distribution.Simple.Setup (ConfigFlags(..))
import           Distribution.Types.Flag (lookupFlagAssignment, mkFlagName)
import           Data.IORef (newIORef, readIORef, writeIORef)
import           System.Directory (doesFileExist)
import           System.Process (callProcess)

main :: IO ()
main = do
  runAutoconfRef <- newIORef False
  let selectHook = do
                     runAutoconf <- readIORef runAutoconfRef
                     if runAutoconf
                     then pure autoconfUserHooks
                     else pure simpleUserHooks
  defaultMainWithHooks
     $ autoconfUserHooks { buildHook = c2hsBuildHooks
                         , haddockHook = c2hsHaddockHooks
                         , replHook = c2hsReplHooks
                         , postConf    = \a c p l -> do
                                           let systemLibArchive = lookupFlagAssignment (mkFlagName "system-libarchive")
                                                                    $ configConfigurationsFlags c
                                           when (maybe False not systemLibArchive) $ do
                                             e <- doesFileExist "configure"
                                             when (not e) $ callProcess "autoreconf" ["-fi"]
                                             writeIORef runAutoconfRef True
                                           selectHook >>= \h -> postConf h a c p l
                         , preBuild    = ((selectHook >>=) .) . flip . flip preBuild
                         , preCopy     = ((selectHook >>=) .) . flip . flip preCopy
                         , preClean    = ((selectHook >>=) .) . flip . flip preClean
                         , preInst     = ((selectHook >>=) .) . flip . flip preInst
                         , preHscolour = ((selectHook >>=) .) . flip . flip preHscolour
                         , preHaddock  = ((selectHook >>=) .) . flip . flip preHaddock
                         , preReg      = ((selectHook >>=) .) . flip . flip preReg
                         , preUnreg    = ((selectHook >>=) .) . flip . flip preUnreg
                         }

