-- | Import this module to get access to the entire C API.
--
-- Functions that
-- are deprecated in the C API are not exposed at all.
module Codec.Archive.Foreign ( module Codec.Archive.Foreign.ArchiveEntry
                             , module Codec.Archive.Foreign.Archive
                             -- * Helper functions
                             , resultToErr
                             ) where

import           Codec.Archive.Foreign.Archive
import           Codec.Archive.Foreign.Archive.Macros
import           Codec.Archive.Foreign.ArchiveEntry
