{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Codec.Archive.Types ( -- * Concrete (Haskell) data types
                             Entry (..)
                           , EntryContent (..)
                           , Ownership (..)
                           , ModTime
                           , Id
                           , Permissions
                           , ArchiveEncryption (..)
                           , ArchiveResult (..)
                           -- * Foreign types
                           , module Codec.Archive.Types.Foreign
                           -- * Callbacks
                           , ArchiveOpenCallback
                           , ArchiveCloseCallback
                           , ArchiveSwitchCallback
                           ) where

import           Codec.Archive.Types.Foreign
import           Control.DeepSeq             (NFData)
import qualified Data.ByteString             as BS
import           Data.Int                    (Int64)
import           Foreign.C.Types             (CLong, CTime)
import           Foreign.Ptr                 (Ptr)
import           GHC.Generics                (Generic)
import           System.Posix.Types          (CMode (..))

type ArchiveOpenCallback a = Ptr Archive -> Ptr a -> IO ArchiveResult
type ArchiveCloseCallback a = Ptr Archive -> Ptr a -> IO ArchiveResult
type ArchiveSwitchCallback a b = Ptr Archive -> Ptr a -> Ptr b -> IO ArchiveResult

data ArchiveResult = ArchiveOk
                   | ArchiveEOF
                   | ArchiveRetry
                   | ArchiveWarn
                   | ArchiveFailed
                   | ArchiveFatal
                   deriving (Eq, Show, Generic, NFData)

data ArchiveEncryption = HasEncryption
                       | NoEncryption
                       | EncryptionUnsupported
                       | EncryptionUnknown

-- TODO: support everything here: http://hackage.haskell.org/package/tar/docs/Codec-Archive-Tar-Entry.html#t:EntryContent
data EntryContent = NormalFile !BS.ByteString
                  | Directory
                  | Symlink !FilePath

data Entry = Entry { filepath    :: !FilePath
                   , content     :: !EntryContent
                   , permissions :: !Permissions
                   , ownership   :: !Ownership
                   , time        :: !(Maybe ModTime)
                   }

data Ownership = Ownership { userName  :: !(Maybe String)
                           , groupName :: !(Maybe String)
                           , ownerId   :: !Id
                           , groupId   :: !Id
                           }

type Permissions = CMode
type ModTime = (CTime, CLong)

-- | A user or group ID
type Id = Int64
