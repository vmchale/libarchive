module Codec.Archive.Internal.Permissions ( standardPermissions
                                 , executablePermissions
                                 ) where

import           Codec.Archive.Internal.Types

standardPermissions :: Permissions
standardPermissions = 0o644

-- | Also used for directories
executablePermissions :: Permissions
executablePermissions = 0o755
