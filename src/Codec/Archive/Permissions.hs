module Codec.Archive.Permissions ( standardPermissions
                                 , executablePermissions
                                 ) where

import           Codec.Archive.Types

standardPermissions :: Permissions
standardPermissions = 0o644

executablePermissions :: Permissions
executablePermissions = 0o755
