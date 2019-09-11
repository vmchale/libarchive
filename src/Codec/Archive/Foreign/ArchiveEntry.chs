-- | Functions found in @archive_entry.h@
--
-- Functions in this module are stateful and hence take place in the 'IO'
-- monad.
module Codec.Archive.Foreign.ArchiveEntry ( -- * Direct bindings (entry)
                                            archiveEntryClear
                                          , archiveEntryClone
                                          , archiveEntryNew
                                          , archiveEntryFree
                                          , archiveEntryNew2
                                          , archiveEntryAtime
                                          , archiveEntryAtimeNsec
                                          , archiveEntryAtimeIsSet
                                          , archiveEntryBirthtime
                                          , archiveEntryBirthtimeNsec
                                          , archiveEntryBirthtimeIsSet
                                          , archiveEntryCtime
                                          , archiveEntryCtimeNsec
                                          , archiveEntryCtimeIsSet
                                          , archiveEntryDev
                                          , archiveEntryDevIsSet
                                          , archiveEntryDevminor
                                          , archiveEntryDevmajor
                                          , archiveEntryFflags
                                          , archiveEntryFflagsText
                                          , archiveEntryFiletype
                                          , archiveEntryGid
                                          , archiveEntryGname
                                          , archiveEntryGnameUtf8
                                          , archiveEntryGnameW
                                          , archiveEntryHardlink
                                          , archiveEntryHardlinkUtf8
                                          , archiveEntryHardlinkW
                                          , archiveEntryIno
                                          , archiveEntryIno64
                                          , archiveEntryInoIsSet
                                          , archiveEntryMode
                                          , archiveEntryMtime
                                          , archiveEntryMtimeNsec
                                          , archiveEntryMtimeIsSet
                                          , archiveEntryNlink
                                          , archiveEntryPathname
                                          , archiveEntryPathnameUtf8
                                          , archiveEntryPathnameW
                                          , archiveEntryPerm
                                          , archiveEntryRdev
                                          , archiveEntryRdevmajor
                                          , archiveEntryRdevminor
                                          , archiveEntrySourcepath
                                          , archiveEntrySourcepathW
                                          , archiveEntrySize
                                          , archiveEntrySizeIsSet
                                          , archiveEntryStrmode
                                          , archiveEntrySymlink
                                          , archiveEntrySymlinkW
                                          , archiveEntrySymlinkUtf8
                                          , archiveEntryUid
                                          , archiveEntryUname
                                          , archiveEntryUnameUtf8
                                          , archiveEntryUnameW
                                          , archiveEntryIsDataEncrypted
                                          , archiveEntryIsMetadataEncrypted
                                          , archiveEntryIsEncrypted
                                          , archiveEntrySetAtime
                                          , archiveEntryUnsetAtime
                                          , archiveEntrySetBirthtime
                                          , archiveEntryUnsetBirthtime
                                          , archiveEntrySetCtime
                                          , archiveEntryUnsetCtime
                                          , archiveEntrySetDev
                                          , archiveEntrySetDevminor
                                          , archiveEntrySetDevmajor
                                          , archiveEntrySetFflags
                                          , archiveEntryCopyFflagsText
                                          , archiveEntryCopyFflagsTextW
                                          , archiveEntrySetFiletype
                                          , archiveEntrySetGid
                                          , archiveEntrySetGname
                                          , archiveEntrySetGnameUtf8
                                          , archiveEntryCopyGname
                                          , archiveEntryCopyGnameW
                                          , archiveEntryUpdateGnameUtf8
                                          , archiveEntrySetHardlink
                                          , archiveEntrySetHardlinkUtf8
                                          , archiveEntryCopyHardlink
                                          , archiveEntryCopyHardlinkW
                                          , archiveEntryUpdateHardlinkUtf8
                                          , archiveEntrySetIno
                                          , archiveEntrySetIno64
                                          , archiveEntrySetLink
                                          , archiveEntrySetLinkUtf8
                                          , archiveEntryCopyLink
                                          , archiveEntryCopyLinkW
                                          , archiveEntryUpdateLinkUtf8
                                          , archiveEntrySetMode
                                          , archiveEntrySetMtime
                                          , archiveEntryUnsetMtime
                                          , archiveEntrySetNlink
                                          , archiveEntrySetPathname
                                          , archiveEntrySetPathnameUtf8
                                          , archiveEntryCopyPathname
                                          , archiveEntryCopyPathnameW
                                          , archiveEntryUpdatePathnameUtf8
                                          , archiveEntrySetPerm
                                          , archiveEntrySetRdev
                                          , archiveEntrySetRdevmajor
                                          , archiveEntrySetRdevminor
                                          , archiveEntrySetSize
                                          , archiveEntryUnsetSize
                                          , archiveEntryCopySourcepath
                                          , archiveEntryCopySourcepathW
                                          , archiveEntrySetSymlink
                                          , archiveEntrySetSymlinkUtf8
                                          , archiveEntryCopySymlink
                                          , archiveEntryCopySymlinkW
                                          , archiveEntryUpdateSymlinkUtf8
                                          , archiveEntrySetUid
                                          , archiveEntrySetUname
                                          , archiveEntrySetUnameUtf8
                                          , archiveEntryCopyUname
                                          , archiveEntryCopyUnameW
                                          , archiveEntryUpdateUnameUtf8
                                          , archiveEntryStat
                                          , archiveEntryCopyStat
                                          , archiveEntryMacMetadata
                                          , archiveEntryCopyMacMetadata
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

import Codec.Archive.Foreign.ArchiveEntry.Macros
import Codec.Archive.Foreign.ArchiveEntry.Raw
import Codec.Archive.Types
import Data.Coerce (coerce)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr (Ptr, castPtr)
import System.PosixCompat.Types (CMode (..))

-- TODO: higher level archiveEntryXattrList?

#include <archive_entry.h>

{#pointer *archive as ArchivePtr -> Archive #}
{#pointer *archive_entry as ArchiveEntryPtr -> ArchiveEntry #}
{#pointer *stat as StatPtr -> Stat #}

{# fun archive_entry_clear as ^ { `ArchiveEntryPtr' } -> `ArchiveEntryPtr' #}
{# fun archive_entry_clone as ^ { `ArchiveEntryPtr' } -> `ArchiveEntryPtr' #}
{# fun archive_entry_new as ^ {} -> `ArchiveEntryPtr' #}
{# fun archive_entry_free as ^ { `ArchiveEntryPtr' } -> `()' #}
{# fun archive_entry_new2 as ^ { `ArchivePtr' } -> `ArchiveEntryPtr' #}
{# fun archive_entry_atime as ^ { `ArchiveEntryPtr' } -> `CTime' fromIntegral #}
{# fun archive_entry_atime_nsec as ^ { `ArchiveEntryPtr' } -> `CLong' #}
{# fun archive_entry_birthtime as ^ { `ArchiveEntryPtr' } -> `CTime' fromIntegral #}
{# fun archive_entry_birthtime_nsec as ^ { `ArchiveEntryPtr' } -> `CLong' #}
{# fun archive_entry_ctime as ^ { `ArchiveEntryPtr' } -> `CTime' fromIntegral #}
{# fun archive_entry_ctime_nsec as ^ { `ArchiveEntryPtr' } -> `CLong' #}
{# fun archive_entry_dev as ^ { `ArchiveEntryPtr' } -> `Dev' id #}
{# fun archive_entry_devminor as ^ { `ArchiveEntryPtr' } -> `Dev' id #}
{# fun archive_entry_devmajor as ^ { `ArchiveEntryPtr' } -> `Dev' id #}
{# fun archive_entry_fflags as ^ { `ArchiveEntryPtr', `CULong', `CULong' } -> `()' #}
{# fun archive_entry_fflags_text as ^ { `ArchiveEntryPtr' } -> `CString' #}
{# fun archive_entry_filetype as ^ { `ArchiveEntryPtr' } -> `FileType' coerce #}
{# fun archive_entry_gid as ^ { `ArchiveEntryPtr' } -> `LaInt64' id #}
{# fun archive_entry_gname as ^ { `ArchiveEntryPtr' } -> `CString' #}
{# fun archive_entry_gname_utf8 as ^ { `ArchiveEntryPtr' } -> `CString' #}
{# fun archive_entry_gname_w as ^ { `ArchiveEntryPtr' } -> `CWString' castPtr #}
{# fun archive_entry_hardlink as ^ { `ArchiveEntryPtr' } -> `CString' #}
{# fun archive_entry_hardlink_utf8 as ^ { `ArchiveEntryPtr' } -> `CString' #}
{# fun archive_entry_hardlink_w as ^ { `ArchiveEntryPtr' } -> `CWString' castPtr #}
{# fun archive_entry_ino as ^ { `ArchiveEntryPtr' } -> `LaInt64' id #}
{# fun archive_entry_ino64 as ^ { `ArchiveEntryPtr' } -> `LaInt64' id #}
{# fun archive_entry_mode as ^ { `ArchiveEntryPtr' } -> `CMode' coerce #}
{# fun archive_entry_mtime as ^ { `ArchiveEntryPtr' } -> `CTime' fromIntegral #}
{# fun archive_entry_mtime_nsec as ^ { `ArchiveEntryPtr' } -> `CLong' #}
{# fun archive_entry_nlink as ^ { `ArchiveEntryPtr' } -> `CUInt' #}
{# fun archive_entry_pathname as ^ { `ArchiveEntryPtr' } -> `CString' #}
{# fun archive_entry_pathname_utf8 as ^ { `ArchiveEntryPtr' } -> `CString' #}
{# fun archive_entry_pathname_w as ^ { `ArchiveEntryPtr' } -> `CWString' castPtr #}
{# fun archive_entry_perm as ^ { `ArchiveEntryPtr' } -> `CMode' coerce #}
{# fun archive_entry_rdev as ^ { `ArchiveEntryPtr' } -> `Dev' id #}
{# fun archive_entry_rdevmajor as ^ { `ArchiveEntryPtr' } -> `Dev' id #}
{# fun archive_entry_rdevminor as ^ { `ArchiveEntryPtr' } -> `Dev' id #}
{# fun archive_entry_sourcepath as ^ { `ArchiveEntryPtr' } -> `CString' #}
{# fun archive_entry_sourcepath_w as ^ { `ArchiveEntryPtr' } -> `CWString' castPtr #}
{# fun archive_entry_size as ^ { `ArchiveEntryPtr' } -> `LaInt64' id #}
{# fun archive_entry_strmode as ^ { `ArchiveEntryPtr' } -> `CString' #}
{# fun archive_entry_symlink as ^ { `ArchiveEntryPtr' } -> `CString' #}
{# fun archive_entry_symlink_w as ^ { `ArchiveEntryPtr' } -> `CWString' castPtr #}
{# fun archive_entry_symlink_utf8 as ^ { `ArchiveEntryPtr' } -> `CString' #}
{# fun archive_entry_uid as ^ { `ArchiveEntryPtr' } -> `LaInt64' id #}
{# fun archive_entry_uname as ^ { `ArchiveEntryPtr' } -> `CString' #}
{# fun archive_entry_uname_utf8 as ^ { `ArchiveEntryPtr' } -> `CString' #}
{# fun archive_entry_uname_w as ^ { `ArchiveEntryPtr' } -> `CWString' castPtr #}
{# fun archive_entry_set_atime as ^ { `ArchiveEntryPtr', coerce `CTime', `CLong' } -> `()' #}
{# fun archive_entry_unset_atime as ^ { `ArchiveEntryPtr' } -> `()' #}
{# fun archive_entry_set_birthtime as ^ { `ArchiveEntryPtr', coerce `CTime', `CLong' } -> `()' #}
{# fun archive_entry_unset_birthtime as ^ { `ArchiveEntryPtr' } -> `()' #}
{# fun archive_entry_set_ctime as ^ { `ArchiveEntryPtr', coerce `CTime', `CLong' } -> `()' #}
{# fun archive_entry_unset_ctime as ^ { `ArchiveEntryPtr' } -> `()' #}
{# fun archive_entry_set_dev as ^ { `ArchiveEntryPtr', id `Dev' } -> `()' #}
{# fun archive_entry_set_devmajor as ^ { `ArchiveEntryPtr', id `Dev' } -> `()' #}
{# fun archive_entry_set_devminor as ^ { `ArchiveEntryPtr', id `Dev' } -> `()' #}
{# fun archive_entry_set_fflags as ^ { `ArchiveEntryPtr', `CULong', `CULong' } -> `()' #}
{# fun archive_entry_copy_fflags_text as ^ { `ArchiveEntryPtr', `CString' } -> `CString' #}
{# fun archive_entry_copy_fflags_text_w as ^ { `ArchiveEntryPtr', castPtr `CWString' } -> `CWString' castPtr #}
{# fun archive_entry_set_filetype as ^ { `ArchiveEntryPtr', coerce `FileType' } -> `()' #}
{# fun archive_entry_set_gid as ^ { `ArchiveEntryPtr', id `LaInt64' } -> `()' #}
{# fun archive_entry_set_gname as ^ { `ArchiveEntryPtr', `CString' } -> `()' #}
{# fun archive_entry_set_gname_utf8 as ^ { `ArchiveEntryPtr', `CString' } -> `()' #}
{# fun archive_entry_copy_gname as ^ { `ArchiveEntryPtr', `CString' } -> `()' #}
{# fun archive_entry_copy_gname_w as ^ { `ArchiveEntryPtr', castPtr `CWString' } -> `()' #}
{# fun archive_entry_set_hardlink as ^ { `ArchiveEntryPtr', `CString' } -> `()' #}
{# fun archive_entry_set_hardlink_utf8 as ^ { `ArchiveEntryPtr', `CString' } -> `()' #}
{# fun archive_entry_copy_hardlink as ^ { `ArchiveEntryPtr', `CString' } -> `()' #}
{# fun archive_entry_copy_hardlink_w as ^ { `ArchiveEntryPtr', castPtr `CWString' } -> `()' #}
{# fun archive_entry_set_ino as ^ { `ArchiveEntryPtr', id `LaInt64' } -> `()' #}
{# fun archive_entry_set_ino64 as ^ { `ArchiveEntryPtr', id `LaInt64' } -> `()' #}
{# fun archive_entry_set_link as ^ { `ArchiveEntryPtr', `CString' } -> `()' #}
{# fun archive_entry_set_link_utf8 as ^ { `ArchiveEntryPtr', `CString' } -> `()' #}
{# fun archive_entry_copy_link as ^ { `ArchiveEntryPtr', `CString' } -> `()' #}
{# fun archive_entry_copy_link_w as ^ { `ArchiveEntryPtr', castPtr `CWString' } -> `()' #}
{# fun archive_entry_set_mode as ^ { `ArchiveEntryPtr', coerce `CMode' } -> `()' #}
{# fun archive_entry_set_mtime as ^ { `ArchiveEntryPtr', coerce `CTime', `CLong' } -> `()' #}
{# fun archive_entry_unset_mtime as ^ { `ArchiveEntryPtr' } -> `()' #}
{# fun archive_entry_set_nlink as ^ { `ArchiveEntryPtr', `CUInt' } -> `()' #}
{# fun archive_entry_set_pathname as ^ { `ArchiveEntryPtr', `CString' } -> `()' #}
{# fun archive_entry_set_pathname_utf8 as ^ { `ArchiveEntryPtr', `CString' } -> `()' #}
{# fun archive_entry_copy_pathname as ^ { `ArchiveEntryPtr', `CString' } -> `()' #}
{# fun archive_entry_copy_pathname_w as ^ { `ArchiveEntryPtr', castPtr `CWString' } -> `()' #}
{# fun archive_entry_set_perm as ^ { `ArchiveEntryPtr', coerce `CMode' } -> `()' #}
{# fun archive_entry_set_rdev as ^ { `ArchiveEntryPtr', id `Dev' } -> `()' #}
{# fun archive_entry_set_rdevmajor as ^ { `ArchiveEntryPtr', id `Dev' } -> `()' #}
{# fun archive_entry_set_rdevminor as ^ { `ArchiveEntryPtr', id `Dev' } -> `()' #}
{# fun archive_entry_set_size as ^ { `ArchiveEntryPtr', id `LaInt64' } -> `()' #}
{# fun archive_entry_unset_size as ^ { `ArchiveEntryPtr' } -> `()' #}
{# fun archive_entry_copy_sourcepath as ^ { `ArchiveEntryPtr', `CString' } -> `()' #}
{# fun archive_entry_copy_sourcepath_w as ^ { `ArchiveEntryPtr', castPtr `CWString' } -> `()' #}
{# fun archive_entry_set_symlink as ^ { `ArchiveEntryPtr', `CString' } -> `()' #}
{# fun archive_entry_set_symlink_utf8 as ^ { `ArchiveEntryPtr', `CString' } -> `()' #}
{# fun archive_entry_copy_symlink as ^ { `ArchiveEntryPtr', `CString' } -> `()' #}
{# fun archive_entry_copy_symlink_w as ^ { `ArchiveEntryPtr', castPtr `CWString' } -> `()' #}
{# fun archive_entry_set_uid as ^ { `ArchiveEntryPtr', id `LaInt64' } -> `()' #}
{# fun archive_entry_set_uname as ^ { `ArchiveEntryPtr', `CString' } -> `()' #}
{# fun archive_entry_set_uname_utf8 as ^ { `ArchiveEntryPtr', `CString' } -> `()' #}
{# fun archive_entry_copy_uname as ^ { `ArchiveEntryPtr', `CString' } -> `()' #}
{# fun archive_entry_copy_uname_w as ^ { `ArchiveEntryPtr', castPtr `CWString' } -> `()' #}
{# fun archive_entry_stat as ^ { `ArchiveEntryPtr' } -> `StatPtr' #}
{# fun archive_entry_copy_stat as ^ { `ArchiveEntryPtr', `StatPtr' } -> `()' #}
{# fun archive_entry_mac_metadata as ^ { `ArchiveEntryPtr', castPtr `Ptr CSize' } -> `Ptr a' castPtr #}
{# fun archive_entry_copy_mac_metadata as ^ { `ArchiveEntryPtr', castPtr `Ptr a', coerce `CSize' } -> `()' #}

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
