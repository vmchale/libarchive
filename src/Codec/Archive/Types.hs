module Codec.Archive.Types ( -- * Abstract data types
                             Archive
                           , ArchiveEntry
                           , Stat
                           , LinkResolver
                           -- * Concrete (Haskell) data types
                           , Entry (..)
                           , EntryContent (..)
                           , Ownership (..)
                           , ModTime
                           , Id
                           , Permissions
                           , ArchiveEncryption (..)
                           -- * Macros
                           , Flags (..)
                           , ArchiveError (..)
                           , ArchiveFilter (..)
                           , ArchiveFormat (..)
                           , FileType (..)
                           , ArchiveCapabilities (..)
                           , ReadDiskFlags (..)
                           , TimeFlag (..)
                           , EntryACL (..)
                           -- * Values
                           , standardPermissions
                           , executablePermissions
                           ) where

import           Data.Bits          (Bits (..))
import qualified Data.ByteString    as BS
import           Data.Int           (Int64)
import           Data.Semigroup
import           Foreign.C.Types    (CInt, CLong, CTime)
import           System.Posix.Types (CMode (..))

-- | Abstract type
data Archive

-- | Abstract type
data ArchiveEntry

data Stat

data LinkResolver

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

standardPermissions :: Permissions
standardPermissions = 0o644

executablePermissions :: Permissions
executablePermissions = 0o755

newtype ArchiveFormat = ArchiveFormat CInt

newtype FileType = FileType CMode
    deriving (Eq)

-- TODO: make this a sum type ?
newtype ArchiveError = ArchiveError CInt
    deriving (Eq)

newtype Flags = Flags CInt

newtype ReadDiskFlags = ReadDiskFlags CInt

newtype TimeFlag = TimeFlag CInt

newtype EntryACL = EntryACL CInt

newtype ArchiveFilter = ArchiveFilter CInt

newtype ArchiveCapabilities = ArchiveCapabilities CInt
    deriving (Eq)

data ArchiveEncryption = HasEncryption
                       | NoEncryption
                       | EncryptionUnsupported
                       | EncryptionUnknown

instance Semigroup ArchiveCapabilities where
    (<>) (ArchiveCapabilities x) (ArchiveCapabilities y) = ArchiveCapabilities (x .|. y)

instance Monoid ArchiveCapabilities where
    mempty = ArchiveCapabilities 0
    mappend = (<>)

instance Semigroup ReadDiskFlags where
    (<>) (ReadDiskFlags x) (ReadDiskFlags y) = ReadDiskFlags (x .|. y)

instance Semigroup Flags where
    (<>) (Flags x) (Flags y) = Flags (x .|. y)

instance Monoid Flags where
    mempty = Flags 0
    mappend = (<>)

instance Semigroup EntryACL where
    (<>) (EntryACL x) (EntryACL y) = EntryACL (x .|. y)

-- TODO: `has` function for EntryACL
