-- | Functions found in @archive_entry.h@
module Codec.Archive.Foreign.ArchiveEntry ( -- * Direct bindings (entry)
                                            archive_entry_clear
                                          , archive_entry_clone
                                          , archive_entry_new
                                          , archive_entry_free
                                          , archive_entry_new2
                                          , archive_entry_atime
                                          , archive_entry_atime_nsec
                                          , archiveEntryATimeIsSet
                                          , archive_entry_birthtime
                                          , archive_entry_birthtime_nsec
                                          , archiveEntryBirthtimeIsSet
                                          , archive_entry_ctime
                                          , archive_entry_ctime_nsec
                                          , archiveEntryCTimeIsSet
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
                                          , archiveEntryMTimeIsSet
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
                                          , archiveEntryUpdateGNameUtf8
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
                                          , archiveEntryUpdateUNameUtf8
                                          , archive_entry_stat
                                          , archive_entry_copy_stat
                                          , archive_entry_mac_metadata
                                          , archive_entry_copy_mac_metadata
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
                                          -- * Abstract types
                                          , ArchiveEntry
                                          , Stat
                                          -- * Lower-level API types
                                          , FileType
                                          , EntryACL
                                          ) where

import Codec.Archive.Foreign.Common
import Codec.Archive.Types
import Control.Composition ((.*))
import Data.Int (Int64)
import Data.Word (Word64)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr (Ptr)
import System.Posix.Types (CMode (..))

#include <archive_entry.h>

-- Archive entry
foreign import ccall unsafe archive_entry_clear :: Ptr ArchiveEntry -> IO (Ptr ArchiveEntry)
foreign import ccall unsafe archive_entry_clone :: Ptr ArchiveEntry -> IO (Ptr ArchiveEntry)
foreign import ccall unsafe archive_entry_free :: Ptr ArchiveEntry -> IO ()
foreign import ccall unsafe archive_entry_new :: IO (Ptr ArchiveEntry)
foreign import ccall unsafe archive_entry_new2 :: Ptr ArchiveEntry -> IO (Ptr ArchiveEntry)

foreign import ccall unsafe archive_entry_atime :: Ptr ArchiveEntry -> IO CTime
foreign import ccall unsafe archive_entry_atime_nsec :: Ptr ArchiveEntry -> IO CLong
foreign import ccall unsafe archive_entry_atime_is_set :: Ptr ArchiveEntry -> IO CInt
foreign import ccall unsafe archive_entry_birthtime :: Ptr ArchiveEntry -> IO CTime
foreign import ccall unsafe archive_entry_birthtime_nsec :: Ptr ArchiveEntry -> IO CLong
foreign import ccall unsafe archive_entry_birthtime_is_set :: Ptr ArchiveEntry -> IO CInt
foreign import ccall unsafe archive_entry_ctime :: Ptr ArchiveEntry -> IO CTime
foreign import ccall unsafe archive_entry_ctime_nsec :: Ptr ArchiveEntry -> IO CLong
foreign import ccall unsafe archive_entry_ctime_is_set :: Ptr ArchiveEntry -> IO CInt
foreign import ccall unsafe archive_entry_dev :: Ptr ArchiveEntry -> IO Word64
foreign import ccall unsafe archive_entry_dev_is_set :: Ptr ArchiveEntry -> IO CInt
foreign import ccall unsafe archive_entry_devmajor :: Ptr ArchiveEntry -> IO Word64
foreign import ccall unsafe archive_entry_devminor :: Ptr ArchiveEntry -> IO Word64
foreign import ccall unsafe archive_entry_filetype :: Ptr ArchiveEntry -> IO FileType
foreign import ccall unsafe archive_entry_fflags :: Ptr ArchiveEntry -> CULong -> CULong -> IO ()
foreign import ccall unsafe archive_entry_fflags_text :: Ptr ArchiveEntry -> IO CString
foreign import ccall unsafe archive_entry_gid :: Ptr ArchiveEntry -> IO Id
foreign import ccall unsafe archive_entry_gname :: Ptr ArchiveEntry -> IO CString
foreign import ccall unsafe archive_entry_gname_utf8 :: Ptr ArchiveEntry -> IO CString
foreign import ccall unsafe archive_entry_gname_w :: Ptr ArchiveEntry -> IO CWString
foreign import ccall unsafe archive_entry_hardlink :: Ptr ArchiveEntry -> IO CString
foreign import ccall unsafe archive_entry_hardlink_utf8 :: Ptr ArchiveEntry -> IO CString
foreign import ccall unsafe archive_entry_hardlink_w :: Ptr ArchiveEntry -> IO CWString
foreign import ccall unsafe archive_entry_ino :: Ptr ArchiveEntry -> IO Int64
foreign import ccall unsafe archive_entry_ino64 :: Ptr ArchiveEntry -> IO Int64
foreign import ccall unsafe archive_entry_ino_is_set :: Ptr ArchiveEntry -> IO CInt
foreign import ccall unsafe archive_entry_mode :: Ptr ArchiveEntry -> IO CMode
foreign import ccall unsafe archive_entry_mtime :: Ptr ArchiveEntry -> IO CTime
foreign import ccall unsafe archive_entry_mtime_nsec :: Ptr ArchiveEntry -> IO CLong
foreign import ccall unsafe archive_entry_mtime_is_set :: Ptr ArchiveEntry -> IO CInt
foreign import ccall unsafe archive_entry_nlink :: Ptr ArchiveEntry -> IO CUInt
foreign import ccall unsafe archive_entry_pathname :: Ptr ArchiveEntry -> IO CString
foreign import ccall unsafe archive_entry_pathname_utf8 :: Ptr ArchiveEntry -> IO CString
foreign import ccall unsafe archive_entry_pathname_w :: Ptr ArchiveEntry -> IO CWString
foreign import ccall unsafe archive_entry_perm :: Ptr ArchiveEntry -> IO CMode
foreign import ccall unsafe archive_entry_rdev :: Ptr ArchiveEntry -> IO Word64
foreign import ccall unsafe archive_entry_rdevmajor :: Ptr ArchiveEntry -> IO Word64
foreign import ccall unsafe archive_entry_rdevminor :: Ptr ArchiveEntry -> IO Word64
foreign import ccall unsafe archive_entry_sourcepath :: Ptr ArchiveEntry -> IO CString
foreign import ccall unsafe archive_entry_sourcepath_w :: Ptr ArchiveEntry -> IO CWString
foreign import ccall unsafe archive_entry_size :: Ptr ArchiveEntry -> IO Int64
foreign import ccall unsafe archive_entry_size_is_set :: Ptr ArchiveEntry -> IO CInt
foreign import ccall unsafe archive_entry_strmode :: Ptr ArchiveEntry -> IO CString
foreign import ccall unsafe archive_entry_symlink :: Ptr ArchiveEntry -> IO CString
foreign import ccall unsafe archive_entry_symlink_utf8 :: Ptr ArchiveEntry -> IO CString
foreign import ccall unsafe archive_entry_symlink_w :: Ptr ArchiveEntry -> IO CWString
foreign import ccall unsafe archive_entry_uid :: Ptr ArchiveEntry -> IO Id
foreign import ccall unsafe archive_entry_uname :: Ptr ArchiveEntry -> IO CString
foreign import ccall unsafe archive_entry_uname_utf8 :: Ptr ArchiveEntry -> IO CString
foreign import ccall unsafe archive_entry_uname_w :: Ptr ArchiveEntry -> IO CWString
foreign import ccall unsafe archive_entry_is_data_encrypted :: Ptr ArchiveEntry -> IO CInt
foreign import ccall unsafe archive_entry_is_metadata_encrypted :: Ptr ArchiveEntry -> IO CInt
foreign import ccall unsafe archive_entry_is_encrypted :: Ptr ArchiveEntry -> IO CInt

foreign import ccall unsafe archive_entry_set_atime :: Ptr ArchiveEntry -> CTime -> CLong -> IO ()
foreign import ccall unsafe archive_entry_unset_atime :: Ptr ArchiveEntry -> IO ()
foreign import ccall unsafe archive_entry_set_birthtime :: Ptr ArchiveEntry -> CTime -> CLong -> IO ()
foreign import ccall unsafe archive_entry_unset_birthtime :: Ptr ArchiveEntry -> IO ()
foreign import ccall unsafe archive_entry_set_ctime :: Ptr ArchiveEntry -> CTime -> CLong -> IO ()
foreign import ccall unsafe archive_entry_unset_ctime :: Ptr ArchiveEntry -> IO ()
foreign import ccall unsafe archive_entry_set_dev :: Ptr ArchiveEntry -> Int64 -> IO ()
foreign import ccall unsafe archive_entry_set_devmajor :: Ptr ArchiveEntry -> Int64 -> IO ()
foreign import ccall unsafe archive_entry_set_devminor :: Ptr ArchiveEntry -> Int64 -> IO ()
foreign import ccall unsafe archive_entry_set_filetype :: Ptr ArchiveEntry -> FileType -> IO ()
foreign import ccall unsafe archive_entry_set_fflags :: Ptr ArchiveEntry -> CULong -> CULong -> IO ()
foreign import ccall unsafe archive_entry_copy_fflags_text :: Ptr ArchiveEntry -> CString -> IO CString
foreign import ccall unsafe archive_entry_copy_fflags_text_w :: Ptr ArchiveEntry -> CWString -> IO CWString
foreign import ccall unsafe archive_entry_set_gid :: Ptr ArchiveEntry -> Id -> IO ()
foreign import ccall unsafe archive_entry_set_gname :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall unsafe archive_entry_set_gname_utf8 :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall unsafe archive_entry_copy_gname :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall unsafe archive_entry_copy_gname_w :: Ptr ArchiveEntry -> CWString -> IO ()
foreign import ccall unsafe archive_entry_update_gname_utf8 :: Ptr ArchiveEntry -> CString -> IO CInt
foreign import ccall unsafe archive_entry_set_hardlink :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall unsafe archive_entry_set_hardlink_utf8 :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall unsafe archive_entry_copy_hardlink :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall unsafe archive_entry_copy_hardlink_w :: Ptr ArchiveEntry -> CWString -> IO ()
foreign import ccall unsafe archive_entry_update_hardlink_utf8 :: Ptr ArchiveEntry -> CString -> IO CInt
foreign import ccall unsafe archive_entry_set_ino :: Ptr ArchiveEntry -> Int64 -> IO ()
foreign import ccall unsafe archive_entry_set_ino64 :: Ptr ArchiveEntry -> Int64 -> IO ()
foreign import ccall unsafe archive_entry_set_link :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall unsafe archive_entry_set_link_utf8 :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall unsafe archive_entry_copy_link :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall unsafe archive_entry_copy_link_w :: Ptr ArchiveEntry -> CWString -> IO ()
foreign import ccall unsafe archive_entry_update_link_utf8 :: Ptr ArchiveEntry -> CString -> IO CInt
foreign import ccall unsafe archive_entry_set_mode :: Ptr ArchiveEntry -> CMode -> IO ()
foreign import ccall unsafe archive_entry_set_mtime :: Ptr ArchiveEntry -> CTime -> CLong -> IO ()
foreign import ccall unsafe archive_entry_unset_mtime :: Ptr ArchiveEntry -> IO ()
foreign import ccall unsafe archive_entry_set_nlink :: Ptr ArchiveEntry -> CUInt -> IO ()
foreign import ccall unsafe archive_entry_set_pathname :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall unsafe archive_entry_set_pathname_utf8 :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall unsafe archive_entry_copy_pathname :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall unsafe archive_entry_copy_pathname_w :: Ptr ArchiveEntry -> CWString -> IO ()
foreign import ccall unsafe archive_entry_update_pathname_utf8 :: Ptr ArchiveEntry -> CString -> IO CInt
foreign import ccall unsafe archive_entry_set_perm :: Ptr ArchiveEntry -> CMode -> IO ()
foreign import ccall unsafe archive_entry_set_rdev :: Ptr ArchiveEntry -> Int64 -> IO ()
foreign import ccall unsafe archive_entry_set_rdevmajor :: Ptr ArchiveEntry -> Int64 -> IO ()
foreign import ccall unsafe archive_entry_set_rdevminor :: Ptr ArchiveEntry -> Int64 -> IO ()
foreign import ccall unsafe archive_entry_set_size :: Ptr ArchiveEntry -> Int64 -> IO ()
foreign import ccall unsafe archive_entry_unset_size :: Ptr ArchiveEntry -> IO ()
foreign import ccall unsafe archive_entry_copy_sourcepath :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall unsafe archive_entry_copy_sourcepath_w :: Ptr ArchiveEntry -> CWString -> IO ()
foreign import ccall unsafe archive_entry_set_symlink :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall unsafe archive_entry_set_symlink_utf8 :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall unsafe archive_entry_copy_symlink :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall unsafe archive_entry_copy_symlink_w :: Ptr ArchiveEntry -> CWString -> IO ()
foreign import ccall unsafe archive_entry_update_symlink_utf8 :: Ptr ArchiveEntry -> CString -> IO CInt
foreign import ccall unsafe archive_entry_set_uid :: Ptr ArchiveEntry -> Id -> IO ()
foreign import ccall unsafe archive_entry_set_uname :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall unsafe archive_entry_set_uname_utf8 :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall unsafe archive_entry_copy_uname :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall unsafe archive_entry_copy_uname_w :: Ptr ArchiveEntry -> CWString -> IO ()
foreign import ccall unsafe archive_entry_update_uname_utf8 :: Ptr ArchiveEntry -> CString -> IO CInt
foreign import ccall unsafe archive_entry_stat :: Ptr ArchiveEntry -> IO (Ptr Stat)
foreign import ccall unsafe archive_entry_copy_stat :: Ptr ArchiveEntry -> Ptr Stat -> IO ()
foreign import ccall unsafe archive_entry_mac_metadata :: Ptr ArchiveEntry -> Ptr CSize -> IO (Ptr a)
foreign import ccall unsafe archive_entry_copy_mac_metadata :: Ptr ArchiveEntry -> Ptr a -> CSize -> IO ()

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

archiveEntryATimeIsSet :: Ptr ArchiveEntry -> IO Bool
archiveEntryATimeIsSet = fmap intToBool . archive_entry_atime_is_set

archiveEntryBirthtimeIsSet :: Ptr ArchiveEntry -> IO Bool
archiveEntryBirthtimeIsSet = fmap intToBool . archive_entry_birthtime_is_set

archiveEntryCTimeIsSet :: Ptr ArchiveEntry -> IO Bool
archiveEntryCTimeIsSet = fmap intToBool . archive_entry_ctime_is_set

archiveEntryDevIsSet :: Ptr ArchiveEntry -> IO Bool
archiveEntryDevIsSet = fmap intToBool . archive_entry_dev_is_set

archiveEntryInoIsSet :: Ptr ArchiveEntry -> IO Bool
archiveEntryInoIsSet = fmap intToBool . archive_entry_ino_is_set

archiveEntryMTimeIsSet :: Ptr ArchiveEntry -> IO Bool
archiveEntryMTimeIsSet = fmap intToBool . archive_entry_mtime_is_set

archiveEntrySizeIsSet :: Ptr ArchiveEntry -> IO Bool
archiveEntrySizeIsSet = fmap intToBool . archive_entry_size_is_set

archiveEntryIsDataEncrypted :: Ptr ArchiveEntry -> IO Bool
archiveEntryIsDataEncrypted = fmap intToBool . archive_entry_is_data_encrypted

archiveEntryIsMetadataEncrypted :: Ptr ArchiveEntry -> IO Bool
archiveEntryIsMetadataEncrypted = fmap intToBool . archive_entry_is_metadata_encrypted

archiveEntryIsEncrypted :: Ptr ArchiveEntry -> IO Bool
archiveEntryIsEncrypted = fmap intToBool . archive_entry_is_encrypted

archiveEntryUpdateGNameUtf8 :: Ptr ArchiveEntry -> CString -> IO Bool
archiveEntryUpdateGNameUtf8 = fmap intToBool .* archive_entry_update_gname_utf8

archiveEntryUpdateHardlinkUtf8 :: Ptr ArchiveEntry -> CString -> IO Bool
archiveEntryUpdateHardlinkUtf8 = fmap intToBool .* archive_entry_update_hardlink_utf8

archiveEntryUpdateLinkUtf8 :: Ptr ArchiveEntry -> CString -> IO Bool
archiveEntryUpdateLinkUtf8 = fmap intToBool .* archive_entry_update_link_utf8

archiveEntryUpdatePathnameUtf8 :: Ptr ArchiveEntry -> CString -> IO Bool
archiveEntryUpdatePathnameUtf8 = fmap intToBool .* archive_entry_update_pathname_utf8

archiveEntryUpdateSymlinkUtf8 :: Ptr ArchiveEntry -> CString -> IO Bool
archiveEntryUpdateSymlinkUtf8 = fmap intToBool .* archive_entry_update_symlink_utf8

archiveEntryUpdateUNameUtf8 :: Ptr ArchiveEntry -> CString -> IO Bool
archiveEntryUpdateUNameUtf8 = fmap intToBool .* archive_entry_update_uname_utf8

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
