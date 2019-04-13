{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Codec.Archive.Foreign.ArchiveEntry.Raw where

import           Codec.Archive.Types
import           Data.Int            (Int64)
import           Data.Word           (Word64)
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr         (Ptr)
import           System.Posix.Types  (CMode (..))

-- Archive entry
foreign import ccall archive_entry_clear :: Ptr ArchiveEntry -> IO (Ptr ArchiveEntry)
foreign import ccall archive_entry_clone :: Ptr ArchiveEntry -> IO (Ptr ArchiveEntry)
foreign import ccall archive_entry_free :: Ptr ArchiveEntry -> IO ()
foreign import ccall archive_entry_new :: IO (Ptr ArchiveEntry)
foreign import ccall archive_entry_new2 :: Ptr ArchiveEntry -> IO (Ptr ArchiveEntry)

foreign import ccall archive_entry_atime :: Ptr ArchiveEntry -> IO CTime
foreign import ccall archive_entry_atime_nsec :: Ptr ArchiveEntry -> IO CLong
foreign import ccall archive_entry_atime_is_set :: Ptr ArchiveEntry -> IO CInt
foreign import ccall archive_entry_birthtime :: Ptr ArchiveEntry -> IO CTime
foreign import ccall archive_entry_birthtime_nsec :: Ptr ArchiveEntry -> IO CLong
foreign import ccall archive_entry_birthtime_is_set :: Ptr ArchiveEntry -> IO CInt
foreign import ccall archive_entry_ctime :: Ptr ArchiveEntry -> IO CTime
foreign import ccall archive_entry_ctime_nsec :: Ptr ArchiveEntry -> IO CLong
foreign import ccall archive_entry_ctime_is_set :: Ptr ArchiveEntry -> IO CInt
foreign import ccall archive_entry_dev :: Ptr ArchiveEntry -> IO Word64
foreign import ccall archive_entry_dev_is_set :: Ptr ArchiveEntry -> IO CInt
foreign import ccall archive_entry_devmajor :: Ptr ArchiveEntry -> IO Word64
foreign import ccall archive_entry_devminor :: Ptr ArchiveEntry -> IO Word64
foreign import ccall archive_entry_filetype :: Ptr ArchiveEntry -> IO FileType
foreign import ccall archive_entry_fflags :: Ptr ArchiveEntry -> CULong -> CULong -> IO ()
foreign import ccall archive_entry_fflags_text :: Ptr ArchiveEntry -> IO CString
foreign import ccall archive_entry_gid :: Ptr ArchiveEntry -> IO Id
foreign import ccall archive_entry_gname :: Ptr ArchiveEntry -> IO CString
foreign import ccall archive_entry_gname_utf8 :: Ptr ArchiveEntry -> IO CString
foreign import ccall archive_entry_gname_w :: Ptr ArchiveEntry -> IO CWString
foreign import ccall archive_entry_hardlink :: Ptr ArchiveEntry -> IO CString
foreign import ccall archive_entry_hardlink_utf8 :: Ptr ArchiveEntry -> IO CString
foreign import ccall archive_entry_hardlink_w :: Ptr ArchiveEntry -> IO CWString
foreign import ccall archive_entry_ino :: Ptr ArchiveEntry -> IO Int64
foreign import ccall archive_entry_ino64 :: Ptr ArchiveEntry -> IO Int64
foreign import ccall archive_entry_ino_is_set :: Ptr ArchiveEntry -> IO CInt
foreign import ccall archive_entry_mode :: Ptr ArchiveEntry -> IO CMode
foreign import ccall archive_entry_mtime :: Ptr ArchiveEntry -> IO CTime
foreign import ccall archive_entry_mtime_nsec :: Ptr ArchiveEntry -> IO CLong
foreign import ccall archive_entry_mtime_is_set :: Ptr ArchiveEntry -> IO CInt
foreign import ccall archive_entry_nlink :: Ptr ArchiveEntry -> IO CUInt
foreign import ccall archive_entry_pathname :: Ptr ArchiveEntry -> IO CString
foreign import ccall archive_entry_pathname_utf8 :: Ptr ArchiveEntry -> IO CString
foreign import ccall archive_entry_pathname_w :: Ptr ArchiveEntry -> IO CWString
foreign import ccall archive_entry_perm :: Ptr ArchiveEntry -> IO CMode
foreign import ccall archive_entry_rdev :: Ptr ArchiveEntry -> IO Word64
foreign import ccall archive_entry_rdevmajor :: Ptr ArchiveEntry -> IO Word64
foreign import ccall archive_entry_rdevminor :: Ptr ArchiveEntry -> IO Word64
foreign import ccall archive_entry_sourcepath :: Ptr ArchiveEntry -> IO CString
foreign import ccall archive_entry_sourcepath_w :: Ptr ArchiveEntry -> IO CWString
foreign import ccall archive_entry_size :: Ptr ArchiveEntry -> IO Int64
foreign import ccall archive_entry_size_is_set :: Ptr ArchiveEntry -> IO CInt
foreign import ccall archive_entry_strmode :: Ptr ArchiveEntry -> IO CString
foreign import ccall archive_entry_symlink :: Ptr ArchiveEntry -> IO CString
foreign import ccall archive_entry_symlink_utf8 :: Ptr ArchiveEntry -> IO CString
foreign import ccall archive_entry_symlink_w :: Ptr ArchiveEntry -> IO CWString
foreign import ccall archive_entry_uid :: Ptr ArchiveEntry -> IO Id
foreign import ccall archive_entry_uname :: Ptr ArchiveEntry -> IO CString
foreign import ccall archive_entry_uname_utf8 :: Ptr ArchiveEntry -> IO CString
foreign import ccall archive_entry_uname_w :: Ptr ArchiveEntry -> IO CWString
foreign import ccall archive_entry_is_data_encrypted :: Ptr ArchiveEntry -> IO CInt
foreign import ccall archive_entry_is_metadata_encrypted :: Ptr ArchiveEntry -> IO CInt
foreign import ccall archive_entry_is_encrypted :: Ptr ArchiveEntry -> IO CInt

foreign import ccall archive_entry_set_atime :: Ptr ArchiveEntry -> CTime -> CLong -> IO ()
foreign import ccall archive_entry_unset_atime :: Ptr ArchiveEntry -> IO ()
foreign import ccall archive_entry_set_birthtime :: Ptr ArchiveEntry -> CTime -> CLong -> IO ()
foreign import ccall archive_entry_unset_birthtime :: Ptr ArchiveEntry -> IO ()
foreign import ccall archive_entry_set_ctime :: Ptr ArchiveEntry -> CTime -> CLong -> IO ()
foreign import ccall archive_entry_unset_ctime :: Ptr ArchiveEntry -> IO ()
foreign import ccall archive_entry_set_dev :: Ptr ArchiveEntry -> Int64 -> IO ()
foreign import ccall archive_entry_set_devmajor :: Ptr ArchiveEntry -> Int64 -> IO ()
foreign import ccall archive_entry_set_devminor :: Ptr ArchiveEntry -> Int64 -> IO ()
foreign import ccall archive_entry_set_filetype :: Ptr ArchiveEntry -> FileType -> IO ()
foreign import ccall archive_entry_set_fflags :: Ptr ArchiveEntry -> CULong -> CULong -> IO ()
foreign import ccall archive_entry_copy_fflags_text :: Ptr ArchiveEntry -> CString -> IO CString
foreign import ccall archive_entry_copy_fflags_text_w :: Ptr ArchiveEntry -> CWString -> IO CWString
foreign import ccall archive_entry_set_gid :: Ptr ArchiveEntry -> Id -> IO ()
foreign import ccall archive_entry_set_gname :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall archive_entry_set_gname_utf8 :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall archive_entry_copy_gname :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall archive_entry_copy_gname_w :: Ptr ArchiveEntry -> CWString -> IO ()
foreign import ccall archive_entry_update_gname_utf8 :: Ptr ArchiveEntry -> CString -> IO CInt
foreign import ccall archive_entry_set_hardlink :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall archive_entry_set_hardlink_utf8 :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall archive_entry_copy_hardlink :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall archive_entry_copy_hardlink_w :: Ptr ArchiveEntry -> CWString -> IO ()
foreign import ccall archive_entry_update_hardlink_utf8 :: Ptr ArchiveEntry -> CString -> IO CInt
foreign import ccall archive_entry_set_ino :: Ptr ArchiveEntry -> Int64 -> IO ()
foreign import ccall archive_entry_set_ino64 :: Ptr ArchiveEntry -> Int64 -> IO ()
foreign import ccall archive_entry_set_link :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall archive_entry_set_link_utf8 :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall archive_entry_copy_link :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall archive_entry_copy_link_w :: Ptr ArchiveEntry -> CWString -> IO ()
foreign import ccall archive_entry_update_link_utf8 :: Ptr ArchiveEntry -> CString -> IO CInt
foreign import ccall archive_entry_set_mode :: Ptr ArchiveEntry -> CMode -> IO ()
foreign import ccall archive_entry_set_mtime :: Ptr ArchiveEntry -> CTime -> CLong -> IO ()
foreign import ccall archive_entry_unset_mtime :: Ptr ArchiveEntry -> IO ()
foreign import ccall archive_entry_set_nlink :: Ptr ArchiveEntry -> CUInt -> IO ()
foreign import ccall archive_entry_set_pathname :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall archive_entry_set_pathname_utf8 :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall archive_entry_copy_pathname :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall archive_entry_copy_pathname_w :: Ptr ArchiveEntry -> CWString -> IO ()
foreign import ccall archive_entry_update_pathname_utf8 :: Ptr ArchiveEntry -> CString -> IO CInt
foreign import ccall archive_entry_set_perm :: Ptr ArchiveEntry -> CMode -> IO ()
foreign import ccall archive_entry_set_rdev :: Ptr ArchiveEntry -> Int64 -> IO ()
foreign import ccall archive_entry_set_rdevmajor :: Ptr ArchiveEntry -> Int64 -> IO ()
foreign import ccall archive_entry_set_rdevminor :: Ptr ArchiveEntry -> Int64 -> IO ()
foreign import ccall archive_entry_set_size :: Ptr ArchiveEntry -> Int64 -> IO ()
foreign import ccall archive_entry_unset_size :: Ptr ArchiveEntry -> IO ()
foreign import ccall archive_entry_copy_sourcepath :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall archive_entry_copy_sourcepath_w :: Ptr ArchiveEntry -> CWString -> IO ()
foreign import ccall archive_entry_set_symlink :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall archive_entry_set_symlink_utf8 :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall archive_entry_copy_symlink :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall archive_entry_copy_symlink_w :: Ptr ArchiveEntry -> CWString -> IO ()
foreign import ccall archive_entry_update_symlink_utf8 :: Ptr ArchiveEntry -> CString -> IO CInt
foreign import ccall archive_entry_set_uid :: Ptr ArchiveEntry -> Id -> IO ()
foreign import ccall archive_entry_set_uname :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall archive_entry_set_uname_utf8 :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall archive_entry_copy_uname :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall archive_entry_copy_uname_w :: Ptr ArchiveEntry -> CWString -> IO ()
foreign import ccall archive_entry_update_uname_utf8 :: Ptr ArchiveEntry -> CString -> IO CInt
foreign import ccall archive_entry_stat :: Ptr ArchiveEntry -> IO (Ptr Stat)
foreign import ccall archive_entry_copy_stat :: Ptr ArchiveEntry -> Ptr Stat -> IO ()
foreign import ccall archive_entry_mac_metadata :: Ptr ArchiveEntry -> Ptr CSize -> IO (Ptr a)
foreign import ccall archive_entry_copy_mac_metadata :: Ptr ArchiveEntry -> Ptr a -> CSize -> IO ()

foreign import ccall archive_entry_acl_clear :: Ptr ArchiveEntry -> IO ()
foreign import ccall archive_entry_acl_add_entry :: Ptr ArchiveEntry -> EntryACL -> EntryACL -> EntryACL -> CInt -> CString -> IO CInt
foreign import ccall archive_entry_acl_add_entry_w :: Ptr ArchiveEntry -> EntryACL -> EntryACL -> EntryACL -> CInt -> CWString -> IO CInt
foreign import ccall archive_entry_acl_reset :: Ptr ArchiveEntry -> EntryACL -> IO CInt
foreign import ccall archive_entry_acl_next :: Ptr ArchiveEntry -> EntryACL -> EntryACL -> EntryACL -> EntryACL -> CInt -> Ptr CString -> IO CInt
-- foreign import ccall archive_entry_acl_next_w :: Ptr ArchiveEntry -> EntryACL -> EntryACL -> EntryACL -> EntryACL -> CInt -> Ptr CWString -> IO ArchiveError

-- foreign import ccall archive_entry_acl_to_text_w :: Ptr ArchiveEntry -> CSize -> EntryACL -> IO CWString
-- foreign import ccall archive_entry_acl_to_text :: Ptr ArchiveEntry -> CSize -> EntryACL -> IO CString
-- foreign import ccall archive_entry_acl_from_text :: Ptr ArchiveEntry -> CString -> EntryACL -> IO ArchiveError
-- foreign import ccall archive_entry_acl_from_text_w :: Ptr ArchiveEntry -> CWString -> EntryACL -> IO ArchiveError
-- foreign import ccall archive_entry_acl_types :: Ptr ArchiveEntry -> IO EntryACL
-- foreign import ccall archive_entry_count :: Ptr ArchiveEntry -> EntryACL -> IO CInt

-- don't bother with archive_entry_acl

foreign import ccall archive_entry_xattr_clear :: Ptr ArchiveEntry -> IO ()
foreign import ccall archive_entry_xattr_add_entry :: Ptr ArchiveEntry -> CString -> Ptr a -> CSize -> IO ()
foreign import ccall archive_entry_xattr_count :: Ptr ArchiveEntry -> IO CInt
foreign import ccall archive_entry_xattr_reset :: Ptr ArchiveEntry -> IO CInt
foreign import ccall archive_entry_xattr_next :: Ptr ArchiveEntry -> Ptr CString -> Ptr (Ptr a) -> Ptr CSize -> IO CInt

foreign import ccall archive_entry_sparse_clear :: Ptr ArchiveEntry -> IO ()
foreign import ccall archive_entry_sparse_add_entry :: Ptr ArchiveEntry -> Int64 -> Int64 -> IO ()
foreign import ccall archive_entry_sparse_count :: Ptr ArchiveEntry -> IO CInt
foreign import ccall archive_entry_sparse_reset :: Ptr ArchiveEntry -> IO CInt
foreign import ccall archive_entry_sparse_next :: Ptr ArchiveEntry -> Ptr Int64 -> Ptr Int64 -> IO CInt

foreign import ccall archive_entry_linkresolver_new :: Ptr LinkResolver
foreign import ccall archive_entry_linkresolver_set_strategy :: Ptr LinkResolver -> ArchiveFormat -> IO ()
foreign import ccall archive_entry_linkresolver_free :: Ptr LinkResolver -> IO ()
foreign import ccall archive_entry_linkify :: Ptr LinkResolver -> Ptr (Ptr ArchiveEntry) -> Ptr (Ptr ArchiveEntry) -> IO ()
foreign import ccall archive_entry_partial_links :: Ptr LinkResolver -> Ptr CUInt -> IO (Ptr ArchiveEntry)
