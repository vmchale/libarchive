module Codec.Archive.Foreign.ArchiveEntry.Macros ( regular
                                                 , symlink
                                                 , socket
                                                 , characterDevice
                                                 , blockDevice
                                                 , directory
                                                 , fifo
                                                 , archiveEntryACLExecute
                                                 , archiveEntryACLWrite
                                                 , archiveEntryACLRead
                                                 , archiveEntryACLReadData
                                                 , archiveEntryACLListData
                                                 , archiveEntryACLWriteData
                                                 , archiveEntryACLAddFile
                                                 , archiveEntryACLAppendData
                                                 , archiveEntryACLAddSubdirectory
                                                 , archiveEntryACLReadNamedAttrs
                                                 , archiveEntryACLWriteNamedAttrs
                                                 , archiveEntryACLDeleteChild
                                                 , archiveEntryACLReadAttributes
                                                 , archiveEntryACLWriteAttributes
                                                 , archiveEntryACLDelete
                                                 , archiveEntryACLReadACL
                                                 , archiveEntryACLWriteACL
                                                 , archiveEntryACLWriteOwner
                                                 , archiveEntryACLSynchronize
                                                 , archiveEntryACLEntryFileInherit
                                                 , archiveEntryACLEntryDirectoryInherit
                                                 , archiveEntryACLEntryNoPropagateInherit
                                                 , archiveEntryACLEntryInheritOnly
                                                 , archiveEntryACLEntrySuccessfulAccess
                                                 , archiveEntryACLEntryFailedAccess
                                                 , archiveEntryACLTypeAccess
                                                 , archiveEntryACLTypeDefault
                                                 , archiveEntryACLTypeAllow
                                                 , archiveEntryACLTypeDeny
                                                 , archiveEntryACLTypeAudit
                                                 , archiveEntryACLTypeAlarm
                                                 , archiveEntryACLUser
                                                 , archiveEntryACLUserObj
                                                 , archiveEntryACLGroup
                                                 , archiveEntryACLGroupObj
                                                 , archiveEntryACLMask
                                                 , archiveEntryACLOther
                                                 , archiveEntryACLEveryone
                                                 , archiveEntryACLStyleExtraID
                                                 , archiveEntryACLStyleMarkDefault
                                                 ) where

import Codec.Archive.Types

#include <archive_entry.h>

-- stupid function to work around some annoying C quirk
mode_t :: Integer -> FileType
mode_t = FileType . fromIntegral . asOctal

-- converts 0020000 to 16384 etc.
asOctal :: Integral a => a -> a
asOctal n | n < 10 = n
          | otherwise = 8 * asOctal (n `div` 10) + n `mod` 10

-- filetype
regular :: FileType
regular = {# const AE_IFREG #}

symlink :: FileType
symlink = {# const AE_IFLNK #}

socket :: FileType
socket = {# const AE_IFSOCK #}

characterDevice :: FileType
characterDevice = {# const AE_IFCHR #}

blockDevice :: FileType
blockDevice = {# const AE_IFBLK #}

directory :: FileType
directory = {# const AE_IFDIR #}

fifo :: FileType
fifo = {# const AE_IFIFO #}

archiveEntryACLExecute :: EntryACL
archiveEntryACLExecute = EntryACL {# const ARCHIVE_ENTRY_ACL_EXECUTE #}

archiveEntryACLWrite :: EntryACL
archiveEntryACLWrite = EntryACL {# const ARCHIVE_ENTRY_ACL_WRITE #}

archiveEntryACLRead :: EntryACL
archiveEntryACLRead = EntryACL {# const ARCHIVE_ENTRY_ACL_READ #}

archiveEntryACLReadData :: EntryACL
archiveEntryACLReadData = EntryACL {# const ARCHIVE_ENTRY_ACL_READ_DATA #}

archiveEntryACLListData :: EntryACL
archiveEntryACLListData = EntryACL {# const ARCHIVE_ENTRY_ACL_LIST_DIRECTORY #}

archiveEntryACLWriteData :: EntryACL
archiveEntryACLWriteData = EntryACL {# const ARCHIVE_ENTRY_ACL_WRITE_DATA #}

archiveEntryACLAddFile :: EntryACL
archiveEntryACLAddFile = EntryACL {# const ARCHIVE_ENTRY_ACL_ADD_FILE #}

archiveEntryACLAppendData :: EntryACL
archiveEntryACLAppendData = EntryACL {# const ARCHIVE_ENTRY_ACL_APPEND_DATA #}

archiveEntryACLAddSubdirectory :: EntryACL
archiveEntryACLAddSubdirectory = EntryACL {# const ARCHIVE_ENTRY_ACL_ADD_SUBDIRECTORY #}

archiveEntryACLReadNamedAttrs :: EntryACL
archiveEntryACLReadNamedAttrs = EntryACL {# const ARCHIVE_ENTRY_ACL_READ_NAMED_ATTRS #}

archiveEntryACLWriteNamedAttrs :: EntryACL
archiveEntryACLWriteNamedAttrs = EntryACL {# const ARCHIVE_ENTRY_ACL_WRITE_NAMED_ATTRS #}

archiveEntryACLDeleteChild :: EntryACL
archiveEntryACLDeleteChild = EntryACL {# const ARCHIVE_ENTRY_ACL_DELETE_CHILD #}

archiveEntryACLReadAttributes :: EntryACL
archiveEntryACLReadAttributes = EntryACL {# const ARCHIVE_ENTRY_ACL_READ_ATTRIBUTES #}

archiveEntryACLWriteAttributes :: EntryACL
archiveEntryACLWriteAttributes = EntryACL {# const ARCHIVE_ENTRY_ACL_WRITE_ATTRIBUTES #}

archiveEntryACLDelete :: EntryACL
archiveEntryACLDelete = EntryACL {# const ARCHIVE_ENTRY_ACL_DELETE #}

archiveEntryACLReadACL :: EntryACL
archiveEntryACLReadACL = EntryACL {# const ARCHIVE_ENTRY_ACL_READ_ACL #}

archiveEntryACLWriteACL :: EntryACL
archiveEntryACLWriteACL = EntryACL {# const ARCHIVE_ENTRY_ACL_WRITE_ACL #}

archiveEntryACLWriteOwner :: EntryACL
archiveEntryACLWriteOwner = EntryACL {# const ARCHIVE_ENTRY_ACL_WRITE_OWNER #}

archiveEntryACLSynchronize :: EntryACL
archiveEntryACLSynchronize = EntryACL {# const ARCHIVE_ENTRY_ACL_SYNCHRONIZE #}

-- archiveEntryACLEntryInherited :: EntryACL
-- archiveEntryACLEntryInherited = EntryACL {# const ARCHIVE_ENTRY_ACL_ENTRY_INHERITED #}

archiveEntryACLEntryFileInherit :: EntryACL
archiveEntryACLEntryFileInherit = EntryACL {# const ARCHIVE_ENTRY_ACL_ENTRY_FILE_INHERIT #}

archiveEntryACLEntryDirectoryInherit :: EntryACL
archiveEntryACLEntryDirectoryInherit = EntryACL {# const ARCHIVE_ENTRY_ACL_ENTRY_DIRECTORY_INHERIT #}

archiveEntryACLEntryNoPropagateInherit :: EntryACL
archiveEntryACLEntryNoPropagateInherit = EntryACL {# const ARCHIVE_ENTRY_ACL_ENTRY_NO_PROPAGATE_INHERIT #}

archiveEntryACLEntryInheritOnly :: EntryACL
archiveEntryACLEntryInheritOnly = EntryACL {# const ARCHIVE_ENTRY_ACL_ENTRY_INHERIT_ONLY #}

archiveEntryACLEntrySuccessfulAccess :: EntryACL
archiveEntryACLEntrySuccessfulAccess = EntryACL {# const ARCHIVE_ENTRY_ACL_ENTRY_SUCCESSFUL_ACCESS #}

archiveEntryACLEntryFailedAccess :: EntryACL
archiveEntryACLEntryFailedAccess = EntryACL {# const ARCHIVE_ENTRY_ACL_ENTRY_FAILED_ACCESS #}

archiveEntryACLTypeAccess :: EntryACL
archiveEntryACLTypeAccess = EntryACL {# const ARCHIVE_ENTRY_ACL_TYPE_ACCESS #}

archiveEntryACLTypeDefault :: EntryACL
archiveEntryACLTypeDefault = EntryACL {# const ARCHIVE_ENTRY_ACL_TYPE_DEFAULT #}

archiveEntryACLTypeAllow :: EntryACL
archiveEntryACLTypeAllow = EntryACL {# const ARCHIVE_ENTRY_ACL_TYPE_ALLOW #}

archiveEntryACLTypeDeny :: EntryACL
archiveEntryACLTypeDeny = EntryACL {# const ARCHIVE_ENTRY_ACL_TYPE_DENY #}

archiveEntryACLTypeAudit :: EntryACL
archiveEntryACLTypeAudit = EntryACL {# const ARCHIVE_ENTRY_ACL_TYPE_AUDIT #}

archiveEntryACLTypeAlarm :: EntryACL
archiveEntryACLTypeAlarm = EntryACL {# const ARCHIVE_ENTRY_ACL_TYPE_ALARM #}

archiveEntryACLUser :: EntryACL
archiveEntryACLUser = EntryACL {# const ARCHIVE_ENTRY_ACL_USER #}

archiveEntryACLUserObj :: EntryACL
archiveEntryACLUserObj = EntryACL {# const ARCHIVE_ENTRY_ACL_USER_OBJ #}

archiveEntryACLGroup :: EntryACL
archiveEntryACLGroup = EntryACL {# const ARCHIVE_ENTRY_ACL_GROUP #}

archiveEntryACLGroupObj :: EntryACL
archiveEntryACLGroupObj = EntryACL {# const ARCHIVE_ENTRY_ACL_GROUP_OBJ #}

archiveEntryACLMask :: EntryACL
archiveEntryACLMask = EntryACL {# const ARCHIVE_ENTRY_ACL_MASK #}

archiveEntryACLOther :: EntryACL
archiveEntryACLOther = EntryACL {# const ARCHIVE_ENTRY_ACL_OTHER #}

archiveEntryACLEveryone :: EntryACL
archiveEntryACLEveryone = EntryACL {# const ARCHIVE_ENTRY_ACL_EVERYONE #}

archiveEntryACLStyleExtraID :: EntryACL
archiveEntryACLStyleExtraID = EntryACL {# const ARCHIVE_ENTRY_ACL_STYLE_EXTRA_ID #}

archiveEntryACLStyleMarkDefault :: EntryACL
archiveEntryACLStyleMarkDefault = EntryACL {# const ARCHIVE_ENTRY_ACL_STYLE_MARK_DEFAULT #}

-- archiveEntryACLStyleSolaris :: EntryACL
-- archiveEntryACLStyleSolaris = EntryACL {# const ARCHIVE_ENTRY_ACL_STYLE_SOLARIS #}

-- archiveEntryACLStyleSeparatorComma :: EntryACL
-- archiveEntryACLStyleSeparatorComma = EntryACL {# const ARCHIVE_ENTRY_ACL_STYLE_SEPARATOR_COMMA #}

-- archiveEntryACLStyleCompact :: EntryACL
-- archiveEntryACLStyleCompact = EntryACL {# const ARCHIVE_ENTRY_ACL_STYLE_COMPACT #}