-- | Everything here is stateful and hence takes place in the 'IO' monad.
--
-- Consult @archive.h@ or @archive_entry.h@ for documentation. Functions that
-- are deprecated in the C API are not exposed here at all.
module Codec.Archive.Foreign ( module Codec.Archive.Foreign.ArchiveEntry
                             , module Codec.Archive.Foreign.Archive
                             ) where

import           Codec.Archive.Foreign.Archive
import           Codec.Archive.Foreign.ArchiveEntry
