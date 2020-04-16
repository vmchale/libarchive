module Codec.Archive.Monad ( handle
                           , ignore
                           , lenient
                           , touchForeignPtrM
                           , runArchiveM
                           , throwArchiveM
                           , unsafeArchiveMToArchiveST
                           , archiveSTToArchiveM
                           -- * Bracketed resources within 'ArchiveM'
                           , withCStringArchiveM
                           , useAsCStringLenArchiveM
                           , allocaBytesArchiveM
                           , bracketM
                           , ArchiveM
                           , ArchiveST
                           ) where

import           Codec.Archive.Types
import           Control.Exception            (bracket, throw)
import           Control.Monad                (void)
import           Control.Monad.Except         (ExceptT, mapExceptT, runExceptT, throwError)
import           Control.Monad.IO.Class
import           Control.Monad.ST.Lazy        (ST)
import qualified Control.Monad.ST.Lazy        as LazyST
import qualified Control.Monad.ST.Lazy.Unsafe as LazyST
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Unsafe       as BS
import           Foreign.C.String
import           Foreign.ForeignPtr           (ForeignPtr, touchForeignPtr)
import           Foreign.Marshal.Alloc        (allocaBytes)
import           Foreign.Ptr                  (Ptr)

type ArchiveM = ExceptT ArchiveResult IO

type ArchiveST s = ExceptT ArchiveResult (ST s)

unsafeArchiveMToArchiveST :: ArchiveM a -> ArchiveST s a
unsafeArchiveMToArchiveST = mapExceptT LazyST.unsafeIOToST

archiveSTToArchiveM :: ArchiveST LazyST.RealWorld a -> ArchiveM a
archiveSTToArchiveM = mapExceptT LazyST.stToIO

touchForeignPtrM :: ForeignPtr a -> ArchiveM ()
touchForeignPtrM = liftIO . touchForeignPtr

-- for things we don't think is going to fail
ignore :: IO ArchiveResult -> ArchiveM ()
ignore = void . liftIO

-- | Throws 'ArchiveResult' on error.
--
-- @since 2.2.5.0
throwArchiveM :: ArchiveM a -> IO a
throwArchiveM = fmap (either throw id) . runArchiveM

runArchiveM :: ArchiveM a -> IO (Either ArchiveResult a)
runArchiveM = runExceptT

-- TODO: ArchiveFailed Writer monad?
-- archive_clear_error
-- | Only fails on 'ArchiveFatal', goes on in case of 'ArchiveFailed'. Make sure
-- to call `archiveClearError' after running this.
lenient :: IO ArchiveResult -> ArchiveM ()
lenient act = do
    res <- liftIO act
    case res of
        ArchiveFatal -> throw res
        ArchiveEOF   -> throw res
        _            -> pure ()

handle :: IO ArchiveResult -> ArchiveM ()
handle act = do
    res <- liftIO act
    case res of
        ArchiveOk    -> pure ()
        ArchiveRetry -> pure ()
        -- FIXME: ArchiveFailed may be ok
        x            -> throwError x

flipExceptIO :: IO (Either a b) -> ExceptT a IO b
flipExceptIO act = do
    res <- liftIO act
    case res of
        Right x -> pure x
        Left y  -> throwError y

genBracket :: (a -> (b -> IO (Either c d)) -> IO (Either c d)) -- ^ Function like 'withCString' we are trying to lift
           -> a -- ^ Fed to @b@
           -> (b -> ExceptT c IO d) -- ^ The action
           -> ExceptT c IO d
genBracket f x = flipExceptIO . f x . (runExceptT .)

allocaBytesArchiveM :: Int -> (Ptr a -> ExceptT b IO c) -> ExceptT b IO c
allocaBytesArchiveM = genBracket allocaBytes

withCStringArchiveM :: String -> (CString -> ExceptT a IO b) -> ExceptT a IO b
withCStringArchiveM = genBracket withCString

useAsCStringLenArchiveM :: BS.ByteString -> (CStringLen -> ExceptT a IO b) -> ExceptT a IO b
useAsCStringLenArchiveM = genBracket BS.unsafeUseAsCStringLen

bracketM :: IO a -- ^ Allocate/aquire a resource
         -> (a -> IO b) -- ^ Free/release a resource (assumed not to fail)
         -> (a -> ArchiveM c)
         -> ArchiveM c
bracketM get free act =
    flipExceptIO $
        bracket get free (runArchiveM.act)
