{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codec.Archive.Types ( -- * Abstract data types
                             Archive
                           , ArchiveEntry
                           -- * Concrete (Haskell) data types
                           , Entry (..)
                           , EntryContent (..)
                           , Ownership (..)
                           , ModTime
                           , Id
                           , Permissions
                           -- * Macros
                           , ExtractFlags (..)
                           , ArchiveError (..)
                           , ArchiveFilter (..)
                           , ArchiveFormat (..)
                           , FileType (..)
                           -- * Values
                           , standardPermissions
                           , executablePermissions
                           ) where

import           Data.Bits          ((.|.))
import qualified Data.ByteString    as BS
import           Data.Int           (Int64)
import           Data.Semigroup
import           Foreign.C.Types    (CInt, CLong, CTime)
import           System.Posix.Types (CMode (..))

-- | Abstract type
data Archive

-- | Abstract type
data ArchiveEntry

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
    deriving (Num)

newtype FileType = FileType CMode
    deriving (Eq, Num)

-- TODO: make this a sum type ?
newtype ArchiveError = ArchiveError CInt
    deriving (Eq, Num)

newtype ExtractFlags = ExtractFlags CInt
    deriving (Num)

newtype ArchiveFilter = ArchiveFilter CInt
    deriving (Num)

instance Semigroup ExtractFlags where
    (<>) (ExtractFlags x) (ExtractFlags y) = ExtractFlags (x .|. y)

instance Monoid ExtractFlags where
    mempty = 0
    mappend = (<>)
