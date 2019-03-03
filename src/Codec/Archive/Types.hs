{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codec.Archive.Types ( Archive
                           , ArchiveEntry
                           , ExtractFlags (..)
                           , ReadResult (..)
                           ) where

import           Data.Bits        ((.|.))
import           Data.Semigroup
import           Foreign.C.Types  (CInt)
import           Foreign.Storable (Storable)

data Archive

data ArchiveEntry

newtype ReadResult = ReadResult CInt
    deriving (Storable, Eq, Num)

newtype ExtractFlags = ExtractFlags CInt
    deriving (Storable, Eq, Num)

instance Semigroup ExtractFlags where
    (<>) (ExtractFlags x) (ExtractFlags y) = ExtractFlags (x .|. y)

instance Monoid ExtractFlags where
    mempty = 0
    mappend = (<>)
