{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codec.Archive.Types ( -- * Abstract data types
                             Archive
                           , ArchiveEntry
                           -- * Concrete (Haskell) data types
                           , Entry (..)
                           -- * Macros
                           , ExtractFlags (..)
                           , ReadResult (..)
                           , ArchiveFilter (..)
                           , FileType (..)
                           ) where

import           Data.Bits       ((.|.))
import qualified Data.ByteString as BS
import           Data.Semigroup
import           Foreign.C.Types (CInt)

-- | Abstract type
data Archive

-- | Abstract type
data ArchiveEntry

data Entry = Entry { filepath :: FilePath
                   , contents :: BS.ByteString
                   , filetype :: FileType
                   }

newtype FileType = FileType CInt
    deriving (Eq, Num)

-- TODO: make this a sum type
newtype ReadResult = ReadResult CInt
    deriving (Eq, Num)

newtype ExtractFlags = ExtractFlags CInt
    deriving (Eq, Num)

newtype ArchiveFilter = ArchiveFilter CInt
    deriving (Num)

instance Semigroup ExtractFlags where
    (<>) (ExtractFlags x) (ExtractFlags y) = ExtractFlags (x .|. y)

instance Monoid ExtractFlags where
    mempty = 0
    mappend = (<>)
