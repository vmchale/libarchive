module Codec.Archive.Types ( -- * Concrete (Haskell) data types
                             Entry (..)
                           , EntryContent (..)
                           , Ownership (..)
                           , ModTime
                           , Id
                           , Permissions
                           , ArchiveEncryption (..)
                           , ArchiveResult (..)
                           , module Codec.Archive.Types.Foreign
                           ) where

import           Codec.Archive.Types.Foreign
import qualified Data.ByteString             as BS
import           Data.Int                    (Int64)
import           Data.Semigroup
import           Foreign.C.Types             (CLong, CTime)
import           System.Posix.Types          (CMode (..))

data ArchiveResult = ArchiveOk
                   | ArchiveEOF
                   | ArchiveRetry
                   | ArchiveWarn
                   | ArchiveFailed
                   | ArchiveFatal

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
                   , time        :: !ModTime
                   }

data Ownership = Ownership { userName  :: !String
                           , groupName :: !String
                           , ownerId   :: !Id
                           , groupId   :: !Id
                           }

type Permissions = CMode
type ModTime = (CTime, CLong)

-- | A user or group ID
type Id = Int64
