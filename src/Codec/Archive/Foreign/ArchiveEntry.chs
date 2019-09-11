-- | Functions found in @archive_entry.h@
--
-- Functions in this module are stateful and hence take place in the 'IO'
-- monad.
module Codec.Archive.Foreign.ArchiveEntry ( -- * Direct bindings (entry)
                                            archive_entry_clear
                                          , archive_entry_clone
                                          , archive_entry_new
                                          , archive_entry_free
                                          , archive_entry_new2
                                          , archive_entry_atime
                                          , archive_entry_atime_nsec
                                          , archiveEntryAtimeIsSet
                                          , archive_entry_birthtime
                                          , archive_entry_birthtime_nsec
                                          , archiveEntryBirthtimeIsSet
                                          , archive_entry_ctime
                                          , archive_entry_ctime_nsec
                                          , archiveEntryCtimeIsSet
                                          , archive_entry_dev
                                          , archiveEntryDevIsSet
                                          , archive_entry_devminor
                                          , archive_entry_devmajor
                                          , archive_entry_fflags
                                          , archive_entry_fflags_text
                                          , archive_entry_filetype
                                          , archive_entry_gid
                                          , archive_entry_gname
                                          , archive_entry_gname_utf8
                                          , archive_entry_gname_w
                                          , archive_entry_hardlink
                                          , archive_entry_hardlink_utf8
                                          , archive_entry_hardlink_w
                                          , archive_entry_ino
                                          , archive_entry_ino64
                                          , archiveEntryInoIsSet
                                          , archive_entry_mode
                                          , archive_entry_mtime
                                          , archive_entry_mtime_nsec
                                          , archiveEntryMtimeIsSet
                                          , archive_entry_nlink
                                          , archive_entry_pathname
                                          , archive_entry_pathname_utf8
                                          , archive_entry_pathname_w
                                          , archive_entry_perm
                                          , archive_entry_rdev
                                          , archive_entry_rdevmajor
                                          , archive_entry_rdevminor
                                          , archive_entry_sourcepath
                                          , archive_entry_sourcepath_w
                                          , archive_entry_size
                                          , archiveEntrySizeIsSet
                                          , archive_entry_strmode
                                          , archive_entry_symlink
                                          , archive_entry_symlink_w
                                          , archive_entry_symlink_utf8
                                          , archive_entry_uid
                                          , archive_entry_uname
                                          , archive_entry_uname_utf8
                                          , archive_entry_uname_w
                                          , archiveEntryIsDataEncrypted
                                          , archiveEntryIsMetadataEncrypted
                                          , archiveEntryIsEncrypted
                                          , archive_entry_set_atime
                                          , archive_entry_unset_atime
                                          , archive_entry_set_birthtime
                                          , archive_entry_unset_birthtime
                                          , archive_entry_set_ctime
                                          , archive_entry_unset_ctime
                                          , archive_entry_set_dev
                                          , archive_entry_set_devmajor
                                          , archive_entry_set_devminor
                                          , archive_entry_set_fflags
                                          , archive_entry_copy_fflags_text
                                          , archive_entry_copy_fflags_text_w
                                          , archive_entry_set_filetype
                                          , archive_entry_set_gid
                                          , archive_entry_set_gname
                                          , archive_entry_set_gname_utf8
                                          , archive_entry_copy_gname
                                          , archive_entry_copy_gname_w
                                          , archiveEntryUpdateGnameUtf8
                                          , archive_entry_set_hardlink
                                          , archive_entry_set_hardlink_utf8
                                          , archive_entry_copy_hardlink
                                          , archive_entry_copy_hardlink_w
                                          , archiveEntryUpdateHardlinkUtf8
                                          , archive_entry_set_ino
                                          , archive_entry_set_ino64
                                          , archive_entry_set_link
                                          , archive_entry_set_link_utf8
                                          , archive_entry_copy_link
                                          , archive_entry_copy_link_w
                                          , archiveEntryUpdateLinkUtf8
                                          , archive_entry_set_mode
                                          , archive_entry_set_mtime
                                          , archive_entry_unset_mtime
                                          , archive_entry_set_nlink
                                          , archive_entry_set_pathname
                                          , archive_entry_set_pathname_utf8
                                          , archive_entry_copy_pathname
                                          , archive_entry_copy_pathname_w
                                          , archiveEntryUpdatePathnameUtf8
                                          , archive_entry_set_perm
                                          , archive_entry_set_rdev
                                          , archive_entry_set_rdevmajor
                                          , archive_entry_set_rdevminor
                                          , archive_entry_set_size
                                          , archive_entry_unset_size
                                          , archive_entry_copy_sourcepath
                                          , archive_entry_copy_sourcepath_w
                                          , archive_entry_set_symlink
                                          , archive_entry_set_symlink_utf8
                                          , archive_entry_copy_symlink
                                          , archive_entry_copy_symlink_w
                                          , archiveEntryUpdateSymlinkUtf8
                                          , archive_entry_set_uid
                                          , archive_entry_set_uname
                                          , archive_entry_set_uname_utf8
                                          , archive_entry_copy_uname
                                          , archive_entry_copy_uname_w
                                          , archiveEntryUpdateUnameUtf8
                                          , archive_entry_stat
                                          , archive_entry_copy_stat
                                          , archive_entry_mac_metadata
                                          , archive_entry_copy_mac_metadata
                                          , archive_entry_acl_next_w
                                          , archive_entry_acl_to_text
                                          , archive_entry_acl_to_text_w
                                          , archive_entry_acl_from_text
                                          , archive_entry_acl_from_text_w
                                          , archive_entry_acl_types
                                          , archive_entry_count
                                          -- * Xattr functions
                                          , archive_entry_xattr_clear
                                          , archive_entry_xattr_add_entry
                                          , archive_entry_xattr_count
                                          , archive_entry_xattr_reset
                                          -- * For sparse archives
                                          , archive_entry_sparse_clear
                                          , archive_entry_sparse_add_entry
                                          , archive_entry_sparse_count
                                          , archive_entry_sparse_reset
                                          -- * Link resolver
                                          , archive_entry_linkresolver_new
                                          , archive_entry_linkresolver_set_strategy
                                          , archive_entry_linkresolver_free
                                          , archive_entry_linkify
                                          , archive_entry_partial_links
                                          -- * ACL
                                          , archive_entry_acl_clear
                                          -- * File types
                                          , regular
                                          , symlink
                                          , socket
                                          , characterDevice
                                          , blockDevice
                                          , directory
                                          , fifo
                                          -- * ACL macros
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
                                          -- * Abstract types
                                          , ArchiveEntry
                                          , Stat
                                          , LinkResolver
                                          -- * Lower-level API types
                                          , FileType
                                          , EntryACL
                                          ) where

{# import qualified Codec.Archive.Types.Foreign #}

import           Codec.Archive.Foreign.ArchiveEntry.Macros
import           Codec.Archive.Foreign.ArchiveEntry.Raw
import           Codec.Archive.Types
import           Foreign.C.String

-- TODO: higher level archiveEntryXattrList?

#include <archive_entry.h>

{#pointer *archive_entry as ArchiveEntryPtr -> ArchiveEntry #}

{# fun archive_entry_atime_is_set as ^ { `ArchiveEntryPtr' } -> `Bool' #}
{# fun archive_entry_birthtime_is_set as ^ { `ArchiveEntryPtr' } -> `Bool' #}
{# fun archive_entry_ctime_is_set as ^ { `ArchiveEntryPtr' } -> `Bool' #}
{# fun archive_entry_dev_is_set as ^ { `ArchiveEntryPtr' } -> `Bool' #}
{# fun archive_entry_ino_is_set as ^ { `ArchiveEntryPtr' } -> `Bool' #}
{# fun archive_entry_mtime_is_set as ^ { `ArchiveEntryPtr' } -> `Bool' #}
{# fun archive_entry_size_is_set as ^ { `ArchiveEntryPtr' } -> `Bool' #}
{# fun archive_entry_is_data_encrypted as ^ { `ArchiveEntryPtr' } -> `Bool' #}
{# fun archive_entry_is_metadata_encrypted as ^ { `ArchiveEntryPtr' } -> `Bool' #}
{# fun archive_entry_is_encrypted as ^ { `ArchiveEntryPtr' } -> `Bool' #}
{# fun archive_entry_update_gname_utf8 as ^ { `ArchiveEntryPtr', `CString' } -> `Bool' #}
{# fun archive_entry_update_hardlink_utf8 as ^ { `ArchiveEntryPtr', `CString' } -> `Bool' #}
{# fun archive_entry_update_link_utf8 as ^ { `ArchiveEntryPtr', `CString' } -> `Bool' #}
{# fun archive_entry_update_pathname_utf8 as ^ { `ArchiveEntryPtr', `CString' } -> `Bool' #}
{# fun archive_entry_update_symlink_utf8 as ^ { `ArchiveEntryPtr', `CString' } -> `Bool' #}
{# fun archive_entry_update_uname_utf8 as ^ { `ArchiveEntryPtr', `CString' } -> `Bool' #}
