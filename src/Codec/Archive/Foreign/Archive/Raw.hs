{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
module Codec.Archive.Foreign.Archive.Raw where

import           Codec.Archive.Types
import           Data.Int            (Int64)
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr         (FunPtr, Ptr)
import           System.Posix.Types  (Fd (..))

-- Miscellaneous
foreign import ccall archive_version_number :: CInt
foreign import ccall archive_version_string :: CString
foreign import ccall archive_version_details :: CString

-- Archive read
foreign import ccall archive_read_new :: IO (Ptr Archive)
foreign import ccall archive_read_support_filter_all :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_filter_bzip2 :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_filter_compress :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_filter_gzip :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_filter_grzip :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_filter_lrzip :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_filter_lz4 :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_filter_lzip :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_filter_lzma :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_filter_lzop :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_filter_none :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_filter_program :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall archive_read_support_filter_program_signature :: Ptr Archive -> CString -> CString -> CSize -> IO ArchiveError
foreign import ccall archive_read_support_filter_rpm :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_filter_uu :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_filter_xz :: Ptr Archive -> IO ArchiveError
-- foreign import ccall archive_read_support_filter_zstd :: Ptr Archive -> IO ArchiveError

foreign import ccall archive_read_support_format_7zip :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_format_all :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_format_ar :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_format_by_code :: Ptr Archive -> CInt -> IO ArchiveError
foreign import ccall archive_read_support_format_cab :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_format_cpio :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_format_empty :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_format_gnutar :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_format_iso9660 :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_format_lha :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_format_mtree :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_format_rar :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_format_raw :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_format_tar :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_format_warc :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_format_xar :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_format_zip :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_format_zip_streamable :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_support_format_zip_seekable :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_set_format :: Ptr Archive -> ArchiveFormat -> IO ArchiveError
foreign import ccall archive_read_append_filter :: Ptr Archive -> ArchiveFilter -> IO ArchiveError
foreign import ccall archive_read_append_filter_program :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall archive_read_append_filter_program_signature :: Ptr Archive -> CString -> Ptr a -> CSize -> IO ArchiveError
foreign import ccall archive_read_set_open_callback :: Ptr Archive -> FunPtr (ArchiveOpenCallbackRaw a) -> IO ArchiveError
foreign import ccall archive_read_set_read_callback :: Ptr Archive -> FunPtr (ArchiveReadCallback a b) -> IO ArchiveError
foreign import ccall archive_read_set_seek_callback :: Ptr Archive -> FunPtr (ArchiveSeekCallback a) -> IO ArchiveError
foreign import ccall archive_read_set_skip_callback :: Ptr Archive -> FunPtr (ArchiveSkipCallback a) -> IO ArchiveError
foreign import ccall archive_read_set_close_callback :: Ptr Archive -> FunPtr (ArchiveCloseCallbackRaw a) -> IO ArchiveError
foreign import ccall archive_read_set_switch_callback :: Ptr Archive -> FunPtr (ArchiveSwitchCallbackRaw a b) -> IO ArchiveError
foreign import ccall archive_read_set_callback_data :: Ptr Archive -> Ptr a -> IO ArchiveError
foreign import ccall archive_read_set_callback_data2 :: Ptr Archive -> Ptr a -> CUInt -> IO ArchiveError
foreign import ccall archive_read_add_callback_data :: Ptr Archive -> Ptr a -> CUInt -> IO ArchiveError
foreign import ccall archive_read_append_callback_data :: Ptr Archive -> Ptr a -> IO ArchiveError
foreign import ccall archive_read_prepend_callback_data :: Ptr Archive -> Ptr a -> IO ArchiveError
foreign import ccall archive_read_open1 :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_open :: Ptr Archive -> Ptr a -> FunPtr (ArchiveOpenCallbackRaw a) -> FunPtr (ArchiveReadCallback a b) -> FunPtr (ArchiveCloseCallbackRaw a) -> IO ArchiveError
foreign import ccall archive_read_open2 :: Ptr Archive -> Ptr a -> FunPtr (ArchiveOpenCallbackRaw a) -> FunPtr (ArchiveReadCallback a b) -> FunPtr (ArchiveSkipCallback a) -> FunPtr (ArchiveCloseCallbackRaw a) -> IO ArchiveError
foreign import ccall archive_read_open_filename :: Ptr Archive -> CString -> CSize -> IO ArchiveError
foreign import ccall archive_read_open_filenames :: Ptr Archive -> Ptr CString -> CSize -> IO ArchiveError
foreign import ccall archive_read_open_filename_w :: Ptr Archive -> CWString -> CSize -> IO ArchiveError
foreign import ccall archive_read_open_memory :: Ptr Archive -> Ptr CChar -> CSize -> IO ArchiveError
foreign import ccall archive_read_open_memory2 :: Ptr Archive -> Ptr a -> CSize -> CSize -> IO ArchiveError
foreign import ccall archive_read_open_fd :: Ptr Archive -> Fd -> CSize -> IO ArchiveError
-- foreign import ccall archive_read_open_FILE
foreign import ccall archive_read_next_header :: Ptr Archive -> Ptr (Ptr ArchiveEntry) -> IO ArchiveError
foreign import ccall archive_read_next_header2 :: Ptr Archive -> Ptr ArchiveEntry -> IO ArchiveError
foreign import ccall archive_read_header_position :: Ptr Archive -> IO Int64
foreign import ccall archive_read_has_encrypted_entries :: Ptr Archive -> IO CInt
foreign import ccall archive_read_format_capabilities :: Ptr Archive -> IO ArchiveCapabilities

foreign import ccall archive_read_data :: Ptr Archive -> Ptr a -> CSize -> IO CSize
foreign import ccall archive_seek_data :: Ptr Archive -> Int64 -> CInt -> IO Int64
foreign import ccall archive_read_data_block :: Ptr Archive -> Ptr (Ptr a) -> Ptr CSize -> Ptr Int64 -> IO ArchiveError
foreign import ccall archive_read_data_skip :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_data_into_fd :: Ptr Archive -> Fd -> IO ArchiveError

foreign import ccall archive_read_set_format_option :: Ptr Archive -> CString -> CString -> CString -> IO ArchiveError
foreign import ccall archive_read_set_filter_option :: Ptr Archive -> CString -> CString -> CString -> IO ArchiveError
foreign import ccall archive_read_set_option :: Ptr Archive -> CString -> CString -> CString -> IO ArchiveError
foreign import ccall archive_read_set_options :: Ptr Archive -> CString -> IO ArchiveError

foreign import ccall archive_read_add_passphrase :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall archive_read_set_passphrase_callback :: Ptr Archive -> Ptr a -> FunPtr (ArchivePassphraseCallback a) -> IO ArchiveError

foreign import ccall archive_read_extract :: Ptr Archive -> Ptr ArchiveEntry -> Flags -> IO ArchiveError
foreign import ccall archive_read_extract2 :: Ptr Archive -> Ptr ArchiveEntry -> Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_extract_set_progress_callback :: Ptr Archive -> FunPtr (Ptr a -> IO ()) -> Ptr a -> IO ()
foreign import ccall archive_read_extract_set_skip_file :: Ptr Archive -> Int64 -> Int64 -> IO ()

foreign import ccall archive_read_close :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_free :: Ptr Archive -> IO ArchiveError

-- Archive write
foreign import ccall archive_write_new :: IO (Ptr Archive)
foreign import ccall archive_write_set_bytes_per_block :: Ptr Archive -> CInt -> IO ArchiveError
foreign import ccall archive_write_get_bytes_per_block :: Ptr Archive -> IO CInt
foreign import ccall archive_write_set_bytes_in_last_block :: Ptr Archive -> CInt -> IO ArchiveError
foreign import ccall archive_write_get_bytes_in_last_block :: Ptr Archive -> IO CInt
foreign import ccall archive_write_set_skip_file :: Ptr Archive -> Int64 -> Int64 -> IO ArchiveError
foreign import ccall archive_write_add_filter :: Ptr Archive -> ArchiveFilter -> IO ArchiveError
foreign import ccall archive_write_add_filter_by_name :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall archive_write_add_filter_b64encode :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_add_filter_bzip2 :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_add_filter_compress :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_add_filter_grzip :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_add_filter_gzip :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_add_filter_lrzip :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_add_filter_lz4 :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_add_filter_lzip :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_add_filter_lzma :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_add_filter_lzop :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_add_filter_none :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_add_filter_program :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall archive_write_add_filter_uuencode :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_add_filter_xz :: Ptr Archive -> IO ArchiveError
-- foreign import ccall archive_write_add_filter_zstd :: Ptr Archive -> IO ArchiveError

foreign import ccall archive_write_set_format :: Ptr Archive -> ArchiveFormat -> IO ArchiveError
foreign import ccall archive_write_set_format_by_name :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall archive_write_set_format_7zip :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_set_format_ar_bsd :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_set_format_ar_svr4 :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_set_format_cpio :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_set_format_cpio_newc :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_set_format_gnutar :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_set_format_iso9660 :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_set_format_mtree :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_set_format_mtree_classic :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_set_format_pax :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_set_format_pax_restricted :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_set_format_raw :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_set_format_shar :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_set_format_shar_dump :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_set_format_ustar :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_set_format_v7tar :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_set_format_warc :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_set_format_xar :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_set_format_zip :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_set_format_filter_by_ext :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall archive_write_set_format_filter_by_ext_def :: Ptr Archive -> CString -> CString -> IO ArchiveError
foreign import ccall archive_write_zip_set_compression_deflate :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_zip_set_compression_store :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_open :: Ptr Archive -> Ptr a -> FunPtr (ArchiveOpenCallbackRaw a) -> FunPtr (ArchiveWriteCallback a b) -> FunPtr (ArchiveCloseCallbackRaw a) -> IO ArchiveError
foreign import ccall archive_write_open_fd :: Ptr Archive -> Fd -> IO ArchiveError
foreign import ccall archive_write_open_filename :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall archive_write_open_filename_w :: Ptr Archive -> CWString -> IO ArchiveError
-- foreign import ccall archive_write_open_FILE
foreign import ccall archive_write_open_memory :: Ptr Archive -> Ptr a -> CSize -> Ptr CSize -> IO ArchiveError

foreign import ccall archive_write_header :: Ptr Archive -> Ptr ArchiveEntry -> IO ArchiveError
foreign import ccall archive_write_data :: Ptr Archive -> Ptr a -> CSize -> IO CSize

foreign import ccall archive_write_data_block :: Ptr Archive -> Ptr a -> CSize -> Int64 -> IO ArchiveError

foreign import ccall archive_write_finish_entry :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_close :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_fail :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_free :: Ptr Archive -> IO ArchiveError

foreign import ccall archive_write_set_format_option :: Ptr Archive -> CString -> CString -> CString -> IO ArchiveError
foreign import ccall archive_write_set_filter_option :: Ptr Archive -> CString -> CString -> CString -> IO ArchiveError
foreign import ccall archive_write_set_option :: Ptr Archive -> CString -> CString -> CString -> IO ArchiveError
foreign import ccall archive_write_set_options :: Ptr Archive -> CString -> IO ArchiveError

foreign import ccall archive_write_set_passphrase :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall archive_write_set_passphrase_callback :: Ptr Archive -> Ptr a -> FunPtr (ArchivePassphraseCallback a) -> IO ArchiveError

foreign import ccall archive_write_disk_new :: IO (Ptr Archive)
foreign import ccall archive_write_disk_set_skip_file :: Ptr Archive -> Int64 -> Int64 -> IO ArchiveError
foreign import ccall archive_write_disk_set_options :: Ptr Archive -> Flags -> IO ArchiveError

foreign import ccall archive_write_disk_set_standard_lookup :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_write_disk_set_group_lookup :: Ptr Archive -> Ptr a -> FunPtr (Ptr a -> CString -> Int64 -> IO Int64) -> FunPtr (Ptr a -> IO ()) -> IO ArchiveError
foreign import ccall archive_write_disk_set_user_lookup :: Ptr Archive -> Ptr a -> FunPtr (Ptr a -> CString -> Int64 -> IO Int64) -> FunPtr (Ptr a -> IO ()) -> IO ArchiveError
foreign import ccall archive_write_disk_gid :: Ptr Archive -> CString -> Int64 -> IO Int64
foreign import ccall archive_write_disk_uid :: Ptr Archive -> CString -> Int64 -> IO Int64

foreign import ccall archive_read_disk_new :: IO (Ptr Archive)
foreign import ccall archive_read_disk_set_symlink_logical :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_disk_set_symlink_physical :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_disk_set_symlink_hybrid :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_disk_entry_from_file :: Ptr Archive -> Ptr ArchiveEntry -> Fd -> Ptr Stat -> IO ArchiveError
foreign import ccall archive_read_disk_gname :: Ptr Archive -> Int64 -> IO CString
foreign import ccall archive_read_disk_uname :: Ptr Archive -> Int64 -> IO CString
foreign import ccall archive_read_disk_set_standard_lookup :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_disk_set_gname_lookup :: Ptr Archive -> Ptr a -> FunPtr (Ptr a -> Int64 -> IO CString) -> FunPtr (Ptr a -> IO ()) -> IO ArchiveError
foreign import ccall archive_read_disk_set_uname_lookup :: Ptr Archive -> Ptr a -> FunPtr (Ptr a -> Int64 -> IO CString) -> FunPtr (Ptr a -> IO ()) -> IO ArchiveError
foreign import ccall archive_read_disk_open :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall archive_read_disk_open_w :: Ptr Archive -> CWString -> IO ArchiveError
foreign import ccall archive_read_disk_descend :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_disk_can_descend :: Ptr Archive -> IO CInt
foreign import ccall archive_read_disk_current_filesystem :: Ptr Archive -> IO CInt
foreign import ccall archive_read_disk_current_filesystem_is_synthetic :: Ptr Archive -> IO CInt
foreign import ccall archive_read_disk_current_filesystem_is_remote :: Ptr Archive -> IO CInt
foreign import ccall archive_read_disk_set_atime_restored :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_read_disk_set_behavior :: Ptr Archive -> ReadDiskFlags -> IO ArchiveError

foreign import ccall archive_read_disk_set_matching :: Ptr Archive -> Ptr Archive -> FunPtr (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO ()) -> Ptr a -> IO ArchiveError
foreign import ccall archive_read_disk_set_metadata_filter_callback :: Ptr Archive -> FunPtr (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO CInt) -> Ptr a -> IO ArchiveError

foreign import ccall archive_free :: Ptr Archive -> IO ArchiveError

foreign import ccall archive_filter_count :: Ptr Archive -> IO CInt
foreign import ccall archive_filter_bytes :: Ptr Archive -> CInt -> Int64
foreign import ccall archive_filter_code :: Ptr Archive -> CInt -> IO Int
foreign import ccall archive_filter_name :: Ptr Archive -> CInt -> IO CString

foreign import ccall archive_errno :: Ptr Archive -> IO CInt
foreign import ccall archive_error_string :: Ptr Archive -> IO CString
foreign import ccall archive_format_name :: Ptr Archive -> IO CString
foreign import ccall archive_format :: Ptr Archive -> IO ArchiveFormat
foreign import ccall archive_clear_error :: Ptr Archive -> IO ()
foreign import ccall archive_set_error :: Ptr Archive -> CInt -> CString -> IO () -- TODO: variadic lol
foreign import ccall archive_copy_error :: Ptr Archive -> Ptr Archive -> IO ()
foreign import ccall archive_file_count :: Ptr Archive -> IO CInt

foreign import ccall archive_match_new :: Ptr Archive
foreign import ccall archive_match_free :: Ptr Archive -> IO ArchiveError
foreign import ccall archive_match_excluded :: Ptr Archive -> IO CInt
foreign import ccall archive_match_path_excluded :: Ptr Archive -> Ptr ArchiveEntry -> IO CInt
foreign import ccall archive_match_exclude_pattern :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall archive_match_exclude_pattern_w :: Ptr Archive -> CWString -> IO ArchiveError
foreign import ccall archive_match_exclude_pattern_from_file :: Ptr Archive -> CString -> CInt -> IO ArchiveError
foreign import ccall archive_match_exclude_pattern_from_file_w :: Ptr Archive -> CWString -> CInt -> IO ArchiveError
foreign import ccall archive_match_include_pattern :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall archive_match_include_pattern_w :: Ptr Archive -> CWString -> IO ArchiveError
foreign import ccall archive_match_include_pattern_from_file :: Ptr Archive -> CString -> CInt -> IO ArchiveError
foreign import ccall archive_match_include_pattern_from_file_w :: Ptr Archive -> CString -> CInt -> IO ArchiveError
foreign import ccall archive_match_path_unmatched_inclusions :: Ptr Archive -> IO CInt
foreign import ccall archive_match_path_unmatched_inclusions_next :: Ptr Archive -> Ptr CString -> IO ArchiveError
foreign import ccall archive_match_path_unmatched_inclusions_next_w :: Ptr Archive -> Ptr CWString -> IO ArchiveError
foreign import ccall archive_match_time_excluded :: Ptr Archive -> Ptr ArchiveEntry -> IO CInt
foreign import ccall archive_match_include_time :: Ptr Archive -> TimeFlag -> CTime -> CLong -> IO ArchiveError
foreign import ccall archive_match_include_date :: Ptr Archive -> TimeFlag -> CString -> IO ArchiveError
foreign import ccall archive_match_include_date_w :: Ptr Archive -> TimeFlag -> CWString -> IO ArchiveError
foreign import ccall archive_match_include_file_time :: Ptr Archive -> TimeFlag -> CString -> IO ArchiveError
foreign import ccall archive_match_include_file_time_w :: Ptr Archive -> TimeFlag -> CWString -> IO ArchiveError
foreign import ccall archive_match_exclude_entry :: Ptr Archive -> TimeFlag -> Ptr ArchiveEntry -> IO ArchiveError
foreign import ccall archive_match_owner_excluded :: Ptr Archive -> Ptr ArchiveEntry -> IO CInt
foreign import ccall archive_match_include_gid :: Ptr Archive -> Id -> IO ArchiveError
foreign import ccall archive_match_include_uid :: Ptr Archive -> Id -> IO ArchiveError
foreign import ccall archive_match_include_uname :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall archive_match_include_uname_w :: Ptr Archive -> CWString -> IO ArchiveError
foreign import ccall archive_match_include_gname :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall archive_match_include_gname_w :: Ptr Archive -> CWString -> IO ArchiveError
