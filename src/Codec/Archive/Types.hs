{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codec.Archive.Types ( -- * Abstract data types
                             Archive
                           , ArchiveEntry
                           -- * Macros
                           , ExtractFlags (..)
                           , ReadResult (..)
                           , ArchiveFilter (..)
                           ) where

import           Data.Bits        ((.|.))
import           Data.Semigroup
import           Foreign.C.Types  (CInt)
import           Foreign.Storable (Storable)

-- | Abstract type
data Archive

-- | Abstract type
data ArchiveEntry

-- TODO: make this a sum type
newtype ReadResult = ReadResult CInt
    deriving (Eq, Num)

newtype ExtractFlags = ExtractFlags CInt
    deriving (Eq, Num)

newtype ArchiveFilter = ArchiveFilter CInt
    deriving (Eq, Num)

instance Semigroup ExtractFlags where
    (<>) (ExtractFlags x) (ExtractFlags y) = ExtractFlags (x .|. y)

instance Monoid ExtractFlags where
    mempty = 0
    mappend = (<>)
