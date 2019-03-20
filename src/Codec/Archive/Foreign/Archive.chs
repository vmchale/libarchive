-- | Module corresponding loosely to @archive.h@
module Codec.Archive.Foreign.Archive ( -- * Direct bindings (read)
                                       archive_read_new
                                     , archive_read_data_skip
                                     , archive_read_data
                                     , archive_read_data_block
                                     , archive_read_free
                                     , archive_read_extract
                                     , archive_read_open_filename
                                     , archive_read_open_filename_w
                                     , archive_read_support_filter_all
                                     , archive_read_support_filter_bzip2
                                     , archive_read_support_filter_compress
                                     , archive_read_support_filter_gzip
                                     , archive_read_support_filter_grzip
                                     , archive_read_support_filter_lrzip
                                     , archive_read_support_filter_lz4
                                     , archive_read_support_filter_lzip
                                     , archive_read_support_filter_lzma
                                     , archive_read_support_filter_lzop
                                     , archive_read_support_filter_none
                                     , archive_read_support_filter_program
                                     , archive_read_support_filter_program_signature
                                     , archive_read_support_filter_rpm
                                     , archive_read_support_filter_uu
                                     , archive_read_support_filter_xz
                                     , archive_read_support_format_7zip
                                     , archive_read_support_format_all
                                     , archive_read_support_format_ar
                                     , archive_read_add_passphrase
                                     , archive_read_set_passphrase_callback
                                     , archive_read_extract2
                                     , archive_read_extract_set_progress_callback
                                     , archive_read_extract_set_skip_file
                                     , archive_read_close
                                     , archive_read_support_format_by_code
                                     , archive_read_support_format_cab
                                     , archive_read_support_format_cpio
                                     , archive_read_support_format_empty
                                     , archive_read_support_format_gnutar
                                     , archive_read_support_format_iso9660
                                     , archive_read_support_format_lha
                                     , archive_read_support_format_mtree
                                     , archive_read_support_format_rar
                                     , archive_read_support_format_raw
                                     , archive_read_support_format_tar
                                     , archive_read_support_format_warc
                                     , archive_read_support_format_xar
                                     , archive_read_support_format_zip
                                     , archive_read_support_format_zip_streamable
                                     , archive_read_support_format_zip_seekable
                                     , archive_read_set_format
                                     , archive_read_append_filter
                                     , archive_read_append_filter_program
                                     , archive_read_append_filter_program_signature
                                     , archive_read_set_open_callback
                                     , archive_read_set_read_callback
                                     , archive_read_set_seek_callback
                                     , archive_read_set_skip_callback
                                     , archive_read_set_close_callback
                                     , archive_read_set_switch_callback
                                     , archive_read_set_callback_data
                                     , archive_read_set_callback_data2
                                     , archive_read_add_callback_data
                                     , archive_read_append_callback_data
                                     , archive_read_prepend_callback_data
                                     , archive_read_open1
                                     , archive_read_open
                                     , archive_read_open2
                                     , archive_read_open_filenames
                                     , archive_read_open_memory
                                     , archive_read_open_memory2
                                     , archive_read_open_fd
                                     , archive_read_next_header
                                     , archive_read_next_header2
                                     , archive_read_header_position
                                     , archiveReadHasEncryptedEntries
                                     , archive_read_format_capabilities
                                     , archive_seek_data
                                     , archive_read_data_into_fd
                                     , archive_read_set_format_option
                                     , archive_read_set_filter_option
                                     , archive_read_set_option
                                     , archive_read_set_options
                                     , archive_read_disk_new
                                     , archive_read_disk_set_symlink_logical
                                     , archive_read_disk_set_symlink_physical
                                     , archive_read_disk_set_symlink_hybrid
                                     , archive_read_disk_entry_from_file
                                     , archive_read_disk_gname
                                     , archive_read_disk_uname
                                     , archive_read_disk_set_standard_lookup
                                     , archive_read_disk_set_gname_lookup
                                     , archive_read_disk_set_uname_lookup
                                     , archive_read_disk_open
                                     , archive_read_disk_open_w
                                     , archive_read_disk_descend
                                     , archiveReadDiskCanDescend
                                     , archive_read_disk_current_filesystem
                                     , archiveReadDiskCurrentFilesystemIsSynthetic
                                     , archiveReadDiskCurrentFilesystemIsRemote
                                     , archive_read_disk_set_atime_restored
                                     , archive_read_disk_set_behavior
                                     , archive_read_disk_set_matching
                                     , archive_read_disk_set_metadata_filter_callback
                                     -- * Direct bindings (write)
                                     , archive_write_set_bytes_per_block
                                     , archive_write_get_bytes_per_block
                                     , archive_write_set_bytes_in_last_block
                                     , archive_write_get_bytes_in_last_block
                                     , archive_write_set_skip_file
                                     , archive_write_add_filter
                                     , archive_write_add_filter_by_name
                                     , archive_write_add_filter_b64encode
                                     , archive_write_add_filter_bzip2
                                     , archive_write_add_filter_compress
                                     , archive_write_add_filter_grzip
                                     , archive_write_add_filter_gzip
                                     , archive_write_add_filter_lrzip
                                     , archive_write_add_filter_lz4
                                     , archive_write_add_filter_lzip
                                     , archive_write_add_filter_lzma
                                     , archive_write_add_filter_lzop
                                     , archive_write_add_filter_none
                                     , archive_write_add_filter_program
                                     , archive_write_add_filter_uuencode
                                     , archive_write_add_filter_xz
                                     , archive_write_data
                                     , archive_write_new
                                     , archive_write_free
                                     , archive_write_set_format_pax_restricted
                                     , archive_write_header
                                     , archive_write_set_format
                                     , archive_write_set_format_by_name
                                     , archive_write_set_format_7zip
                                     , archive_write_set_format_ar_bsd
                                     , archive_write_set_format_ar_svr4
                                     , archive_write_set_format_cpio
                                     , archive_write_set_format_cpio_newc
                                     , archive_write_set_format_gnutar
                                     , archive_write_set_format_iso9660
                                     , archive_write_set_format_mtree
                                     , archive_write_set_format_mtree_classic
                                     , archive_write_set_format_pax
                                     , archive_write_set_format_raw
                                     , archive_write_set_format_shar
                                     , archive_write_set_format_shar_dump
                                     , archive_write_set_format_ustar
                                     , archive_write_set_format_v7tar
                                     , archive_write_set_format_warc
                                     , archive_write_set_format_xar
                                     , archive_write_set_format_zip
                                     , archive_write_set_format_filter_by_ext
                                     , archive_write_set_format_filter_by_ext_def
                                     , archive_write_zip_set_compression_deflate
                                     , archive_write_zip_set_compression_store
                                     , archive_write_open
                                     , archive_write_open_fd
                                     , archive_write_open_filename
                                     , archive_write_open_filename_w
                                     , archive_write_open_memory
                                     , archive_write_data_block
                                     , archive_write_finish_entry
                                     , archive_write_close
                                     , archive_write_fail
                                     , archive_write_set_format_option
                                     , archive_write_set_filter_option
                                     , archive_write_set_option
                                     , archive_write_set_options
                                     , archive_write_set_passphrase
                                     , archive_write_set_passphrase_callback
                                     , archive_write_disk_new
                                     , archive_write_disk_set_skip_file
                                     , archive_write_disk_set_options
                                     , archive_write_disk_set_standard_lookup
                                     , archive_write_disk_set_group_lookup
                                     , archive_write_disk_set_user_lookup
                                     , archive_write_disk_gid
                                     , archive_write_disk_uid
                                     -- * Direct bindings (archive error)
                                     , archive_errno
                                     , archive_error_string
                                     , archive_format_name
                                     , archive_format
                                     , archive_clear_error
                                     , archive_set_error
                                     , archive_copy_error
                                     , archive_file_count
                                     -- * Direct bindings (archive match)
                                     , archive_match
                                     , archive_match_free
                                     , archiveMatchExcluded
                                     , archiveMatchPathExcluded
                                     , archive_match_exclude_pattern
                                     , archive_match_exclude_pattern_w
                                     , archiveMatchExcludePatternFromFile
                                     , archiveMatchExcludePatternFromFileW
                                     , archive_match_include_pattern
                                     , archive_match_include_pattern_w
                                     , archiveMatchIncludePatternFromFile
                                     , archiveMatchIncludePatternFromFileW
                                     , archive_match_path_unmatched_inclusions
                                     , archive_match_path_unmatched_inclusions_next
                                     , archive_match_path_unmatched_inclusions_next_w
                                     , archiveMatchTimeExcluded
                                     , archive_match_include_time
                                     , archive_match_include_date
                                     , archive_match_include_date_w
                                     , archive_match_include_file_time
                                     , archive_match_include_file_time_w
                                     , archive_match_exclude_entry
                                     , archiveMatchOwnerExcluded
                                     , archive_match_include_uid
                                     , archive_match_include_gid
                                     , archive_match_include_uname
                                     , archive_match_include_uname_w
                                     , archive_match_include_gname
                                     , archive_match_include_gname_w
                                     -- * Direct bindings (version/filter/miscellaneous)
                                     , archive_version_number
                                     , archive_version_string
                                     , archive_version_details
                                     , archive_free
                                     , archive_filter_count
                                     , archive_filter_bytes
                                     , archive_filter_code
                                     , archive_filter_name
                                     -- * Version macros
                                     , archiveVersionNumber
                                     , archiveVersionOnlyString
                                     , archiveVersionString
                                     -- * Capability macros
                                     , archiveReadFormatCapsNone
                                     , archiveReadFormatCapsEncryptData
                                     , archiveReadFormatCapsEncryptMetadata
                                     -- * Header read macros
                                     , archiveOk
                                     , archiveEOF
                                     , archiveRetry
                                     , archiveWarn
                                     , archiveFailed
                                     , archiveFatal
                                     -- * Time matching macros
                                     , archiveMatchMTime
                                     , archiveMatchCTime
                                     , archiveMatchNewer
                                     , archiveMatchOlder
                                     , archiveMatchEqual
                                     -- * Entry flags
                                     , archiveExtractOwner
                                     , archiveExtractPerm
                                     , archiveExtractTime
                                     , archiveExtractNoOverwrite
                                     , archiveExtractUnlink
                                     , archiveExtractACL
                                     , archiveExtractFFlags
                                     , archiveExtractXattr
                                     , archiveExtractSecureSymlinks
                                     , archiveExtractSecureNoDotDot
                                     , archiveExtractNoAutodir
                                     , archiveExtractSparse
                                     , archiveExtractMacMetadata
                                     , archiveExtractNoHfsCompression
                                     , archiveExtractHfsCompressionForced
                                     , archiveExtractSecureNoAbsolutePaths
                                     , archiveExtractClearNoChangeFFlags
                                     -- * Filters
                                     , archiveFilterNone
                                     , archiveFilterGzip
                                     , archiveFilterBzip2
                                     , archiveFilterCompress
                                     , archiveFilterProgram
                                     , archiveFilterLzma
                                     , archiveFilterXz
                                     , archiveFilterUu
                                     , archiveFilterRpm
                                     , archiveFilterLzip
                                     , archiveFilterLrzip
                                     , archiveFilterLzop
                                     , archiveFilterGrzip
                                     , archiveFilterLz4
                                     -- * Formats
                                     , archiveFormatCpio
                                     , archiveFormatShar
                                     , archiveFormatTar
                                     , archiveFormatIso9660
                                     , archiveFormatZip
                                     , archiveFormatEmpty
                                     , archiveFormatAr
                                     , archiveFormatMtree
                                     , archiveFormatRaw
                                     , archiveFormatXar
                                     , archiveFormatLha
                                     , archiveFormatCab
                                     , archiveFormatRar
                                     , archiveFormat7zip
                                     , archiveFormatWarc
                                     -- * Read disk flags
                                     , archiveReadDiskRestoreATime
                                     , archiveReadDiskHonorNoDump
                                     , archiveReadDiskMacCopyFile
                                     , archiveReadDiskNoTraverseMounts
                                     , archiveReadDiskNoXattr
                                     -- * Abstract types
                                     , Archive
                                     -- * Haskell types
                                     , ArchiveEncryption (..)
                                     -- * Lower-level API types
                                     , ArchiveError
                                     , Flags
                                     , ArchiveFilter
                                     , ArchiveFormat
                                     , ArchiveCapabilities
                                     , ReadDiskFlags
                                     , TimeFlag
                                     -- * Callback types
                                     , ArchiveReadCallback
                                     , ArchiveSkipCallback
                                     , ArchiveSeekCallback
                                     , ArchiveWriteCallback
                                     , ArchiveOpenCallback
                                     , ArchiveCloseCallback
                                     , ArchiveSwitchCallback
                                     , ArchivePassphraseCallback
                                     -- * Callback constructors
                                     , mkReadCallback
                                     , mkSkipCallback
                                     , mkSeekCallback
                                     , mkWriteCallback
                                     , mkOpenCallback
                                     , mkCloseCallback
                                     , mkSwitchCallback
                                     , mkPassphraseCallback
                                     , mkWriteLookup
                                     , mkReadLookup
                                     , mkCleanup
                                     , mkMatch
                                     , mkFilter
                                     ) where

import Codec.Archive.Foreign.Common
import Control.Composition ((.*), (.**))
import Data.Bits (Bits (..))
import Data.Int (Int64)
import Codec.Archive.Types
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr (FunPtr, Ptr)
import System.Posix.Types (Fd (..))

-- Miscellaneous
foreign import ccall archive_version_number :: CInt
foreign import ccall archive_version_string :: CString
foreign import ccall archive_version_details :: CString

-- destructors: use "dynamic" instead of "wrapper" (but we don't want that)
-- callbacks
foreign import ccall "wrapper" mkReadCallback :: ArchiveReadCallback a b -> IO (FunPtr (ArchiveReadCallback a b))
foreign import ccall "wrapper" mkSkipCallback :: ArchiveSkipCallback a -> IO (FunPtr (ArchiveSkipCallback a))
foreign import ccall "wrapper" mkSeekCallback :: ArchiveSeekCallback a -> IO (FunPtr (ArchiveSeekCallback a))
foreign import ccall "wrapper" mkWriteCallback :: ArchiveWriteCallback a b -> IO (FunPtr (ArchiveWriteCallback a b))
foreign import ccall "wrapper" mkOpenCallback :: ArchiveOpenCallback a -> IO (FunPtr (ArchiveOpenCallback a))
foreign import ccall "wrapper" mkCloseCallback :: ArchiveCloseCallback a -> IO (FunPtr (ArchiveCloseCallback a))
foreign import ccall "wrapper" mkSwitchCallback :: ArchiveSwitchCallback a b -> IO (FunPtr (ArchiveSwitchCallback a b))
foreign import ccall "wrapper" mkPassphraseCallback :: ArchivePassphraseCallback a -> IO (FunPtr (ArchivePassphraseCallback a))

foreign import ccall "wrapper" mkWriteLookup :: (Ptr a -> CString -> Int64 -> IO Int64) -> IO (FunPtr (Ptr a -> CString -> Int64 -> IO Int64))
foreign import ccall "wrapper" mkReadLookup :: (Ptr a -> Int64 -> IO CString) -> IO (FunPtr (Ptr a -> Int64 -> IO CString))
foreign import ccall "wrapper" mkCleanup :: (Ptr a -> IO ()) -> IO (FunPtr (Ptr a -> IO ()))

foreign import ccall "wrapper" mkMatch :: (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO ()) -> IO (FunPtr (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO ()))
foreign import ccall "wrapper" preMkFilter :: (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO CInt) -> IO (FunPtr (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO CInt))

mkFilter :: (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO Bool) -> IO (FunPtr (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO CInt))
mkFilter f = let f' = fmap boolToInt .** f in preMkFilter f'

boolToInt :: Integral a => Bool -> a
boolToInt False = 0
boolToInt True = 1

type ArchiveReadCallback a b = Ptr Archive -> Ptr a -> Ptr (Ptr b) -> IO CSize
type ArchiveSkipCallback a = Ptr Archive -> Ptr a -> Int64 -> IO Int64
type ArchiveSeekCallback a = Ptr Archive -> Ptr a -> Int64 -> CInt -> IO Int64
type ArchiveWriteCallback a b = Ptr Archive -> Ptr a -> Ptr b -> CSize -> IO CSize
type ArchiveOpenCallback a = Ptr Archive -> Ptr a -> IO ArchiveError
type ArchiveCloseCallback a = Ptr Archive -> Ptr a -> IO ArchiveError
type ArchiveSwitchCallback a b = Ptr Archive -> Ptr a -> Ptr b -> IO ArchiveError
type ArchivePassphraseCallback a = Ptr Archive -> Ptr a -> IO CString

-- Archive read
foreign import ccall unsafe archive_read_new :: IO (Ptr Archive)
foreign import ccall unsafe archive_read_support_filter_all :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_filter_bzip2 :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_filter_compress :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_filter_gzip :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_filter_grzip :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_filter_lrzip :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_filter_lz4 :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_filter_lzip :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_filter_lzma :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_filter_lzop :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_filter_none :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_filter_program :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall unsafe archive_read_support_filter_program_signature :: Ptr Archive -> CString -> CString -> CSize -> IO ArchiveError
foreign import ccall unsafe archive_read_support_filter_rpm :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_filter_uu :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_filter_xz :: Ptr Archive -> IO ArchiveError
-- foreign import ccall unsafe archive_read_support_filter_zstd :: Ptr Archive -> IO ArchiveError

foreign import ccall unsafe archive_read_support_format_7zip :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_format_all :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_format_ar :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_format_by_code :: Ptr Archive -> CInt -> IO ArchiveError
foreign import ccall unsafe archive_read_support_format_cab :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_format_cpio :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_format_empty :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_format_gnutar :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_format_iso9660 :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_format_lha :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_format_mtree :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_format_rar :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_format_raw :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_format_tar :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_format_warc :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_format_xar :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_format_zip :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_format_zip_streamable :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_support_format_zip_seekable :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_set_format :: Ptr Archive -> ArchiveFormat -> IO ArchiveError
foreign import ccall unsafe archive_read_append_filter :: Ptr Archive -> ArchiveFilter -> IO ArchiveError
foreign import ccall unsafe archive_read_append_filter_program :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall unsafe archive_read_append_filter_program_signature :: Ptr Archive -> CString -> Ptr a -> CSize -> IO ArchiveError
foreign import ccall unsafe archive_read_set_open_callback :: Ptr Archive -> FunPtr (ArchiveOpenCallback a) -> IO ArchiveError
foreign import ccall unsafe archive_read_set_read_callback :: Ptr Archive -> FunPtr (ArchiveOpenCallback a) -> IO ArchiveError
foreign import ccall unsafe archive_read_set_seek_callback :: Ptr Archive -> FunPtr (ArchiveOpenCallback a) -> IO ArchiveError
foreign import ccall unsafe archive_read_set_skip_callback :: Ptr Archive -> FunPtr (ArchiveSkipCallback a) -> IO ArchiveError
foreign import ccall unsafe archive_read_set_close_callback :: Ptr Archive -> FunPtr (ArchiveCloseCallback a) -> IO ArchiveError
foreign import ccall unsafe archive_read_set_switch_callback :: Ptr Archive -> FunPtr (ArchiveSwitchCallback a b) -> IO ArchiveError
foreign import ccall unsafe archive_read_set_callback_data :: Ptr Archive -> Ptr a -> IO ArchiveError
foreign import ccall unsafe archive_read_set_callback_data2 :: Ptr Archive -> Ptr a -> CUInt -> IO ArchiveError
foreign import ccall unsafe archive_read_add_callback_data :: Ptr Archive -> Ptr a -> CUInt -> IO ArchiveError
foreign import ccall unsafe archive_read_append_callback_data :: Ptr Archive -> Ptr a -> IO ArchiveError
foreign import ccall unsafe archive_read_prepend_callback_data :: Ptr Archive -> Ptr a -> IO ArchiveError
foreign import ccall unsafe archive_read_open1 :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_open :: Ptr Archive -> Ptr a -> FunPtr (ArchiveOpenCallback a) -> FunPtr (ArchiveReadCallback a b) -> FunPtr (ArchiveCloseCallback a) -> IO ArchiveError
foreign import ccall unsafe archive_read_open2 :: Ptr Archive -> Ptr a -> FunPtr (ArchiveOpenCallback a) -> FunPtr (ArchiveReadCallback a b) -> FunPtr (ArchiveSkipCallback a) -> FunPtr (ArchiveCloseCallback a) -> IO ArchiveError
foreign import ccall unsafe archive_read_open_filename :: Ptr Archive -> CString -> CSize -> IO ArchiveError
foreign import ccall unsafe archive_read_open_filenames :: Ptr Archive -> Ptr CString -> CSize -> IO ArchiveError
foreign import ccall unsafe archive_read_open_filename_w :: Ptr Archive -> CWString -> CSize -> IO ArchiveError
foreign import ccall unsafe archive_read_open_memory :: Ptr Archive -> Ptr CChar -> CSize -> IO ArchiveError
foreign import ccall unsafe archive_read_open_memory2 :: Ptr Archive -> Ptr a -> CSize -> CSize -> IO ArchiveError
foreign import ccall unsafe archive_read_open_fd :: Ptr Archive -> Fd -> CSize -> IO ArchiveError
-- foreign import ccall unsafe archive_read_open_FILE 
foreign import ccall unsafe archive_read_next_header :: Ptr Archive -> Ptr (Ptr ArchiveEntry) -> IO ArchiveError
foreign import ccall unsafe archive_read_next_header2 :: Ptr Archive -> Ptr ArchiveEntry -> IO ArchiveError
foreign import ccall unsafe archive_read_header_position :: Ptr Archive -> IO Int64
foreign import ccall unsafe archive_read_has_encrypted_entries :: Ptr Archive -> IO CInt
foreign import ccall unsafe archive_read_format_capabilities :: Ptr Archive -> IO ArchiveCapabilities

foreign import ccall unsafe archive_read_data :: Ptr Archive -> Ptr a -> CSize -> IO CSize
foreign import ccall unsafe archive_seek_data :: Ptr Archive -> Int64 -> CInt -> IO Int64
foreign import ccall unsafe archive_read_data_block :: Ptr Archive -> Ptr (Ptr a) -> Ptr CSize -> Ptr Int64 -> IO ArchiveError
foreign import ccall unsafe archive_read_data_skip :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_data_into_fd :: Ptr Archive -> Fd -> IO ArchiveError

foreign import ccall unsafe archive_read_set_format_option :: Ptr Archive -> CString -> CString -> CString -> IO ArchiveError
foreign import ccall unsafe archive_read_set_filter_option :: Ptr Archive -> CString -> CString -> CString -> IO ArchiveError
foreign import ccall unsafe archive_read_set_option :: Ptr Archive -> CString -> CString -> CString -> IO ArchiveError
foreign import ccall unsafe archive_read_set_options :: Ptr Archive -> CString -> IO ArchiveError

foreign import ccall unsafe archive_read_add_passphrase :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall unsafe archive_read_set_passphrase_callback :: Ptr Archive -> Ptr a -> FunPtr (ArchivePassphraseCallback a) -> IO ArchiveError

foreign import ccall unsafe archive_read_extract :: Ptr Archive -> Ptr ArchiveEntry -> Flags -> IO ArchiveError
foreign import ccall unsafe archive_read_extract2 :: Ptr Archive -> Ptr ArchiveEntry -> Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_extract_set_progress_callback :: Ptr Archive -> (FunPtr (Ptr a -> IO ())) -> Ptr a -> IO ()
foreign import ccall unsafe archive_read_extract_set_skip_file :: Ptr Archive -> Int64 -> Int64 -> IO ()

foreign import ccall unsafe archive_read_close :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_free :: Ptr Archive -> IO ArchiveError

-- Archive write
foreign import ccall unsafe archive_write_new :: IO (Ptr Archive)
foreign import ccall unsafe archive_write_set_bytes_per_block :: Ptr Archive -> CInt -> IO ArchiveError
foreign import ccall unsafe archive_write_get_bytes_per_block :: Ptr Archive -> IO CInt
foreign import ccall unsafe archive_write_set_bytes_in_last_block :: Ptr Archive -> CInt -> IO ArchiveError
foreign import ccall unsafe archive_write_get_bytes_in_last_block :: Ptr Archive -> IO CInt
foreign import ccall unsafe archive_write_set_skip_file :: Ptr Archive -> Int64 -> Int64 -> IO ArchiveError
foreign import ccall unsafe archive_write_add_filter :: Ptr Archive -> ArchiveFilter -> IO ArchiveError
foreign import ccall unsafe archive_write_add_filter_by_name :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall unsafe archive_write_add_filter_b64encode :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_add_filter_bzip2 :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_add_filter_compress :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_add_filter_grzip :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_add_filter_gzip :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_add_filter_lrzip :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_add_filter_lz4 :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_add_filter_lzip :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_add_filter_lzma :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_add_filter_lzop :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_add_filter_none :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_add_filter_program :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall unsafe archive_write_add_filter_uuencode :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_add_filter_xz :: Ptr Archive -> IO ArchiveError
-- foreign import ccall unsafe archive_write_add_filter_zstd :: Ptr Archive -> IO ArchiveError

foreign import ccall unsafe archive_write_set_format :: Ptr Archive -> ArchiveFormat -> IO ArchiveError
foreign import ccall unsafe archive_write_set_format_by_name :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall unsafe archive_write_set_format_7zip :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_set_format_ar_bsd :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_set_format_ar_svr4 :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_set_format_cpio :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_set_format_cpio_newc :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_set_format_gnutar :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_set_format_iso9660 :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_set_format_mtree :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_set_format_mtree_classic :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_set_format_pax :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_set_format_pax_restricted :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_set_format_raw :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_set_format_shar :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_set_format_shar_dump :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_set_format_ustar :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_set_format_v7tar :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_set_format_warc :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_set_format_xar :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_set_format_zip :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_set_format_filter_by_ext :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall unsafe archive_write_set_format_filter_by_ext_def :: Ptr Archive -> CString -> CString -> IO ArchiveError
foreign import ccall unsafe archive_write_zip_set_compression_deflate :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_zip_set_compression_store :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_open :: Ptr Archive -> Ptr a -> FunPtr (ArchiveOpenCallback a) -> FunPtr (ArchiveWriteCallback a b) -> FunPtr (ArchiveCloseCallback a) -> IO ArchiveError
foreign import ccall unsafe archive_write_open_fd :: Ptr Archive -> Fd -> IO ArchiveError
foreign import ccall unsafe archive_write_open_filename :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall unsafe archive_write_open_filename_w :: Ptr Archive -> CWString -> IO ArchiveError
-- foreign import ccall unsafe archive_write_open_FILE
foreign import ccall unsafe archive_write_open_memory :: Ptr Archive -> Ptr a -> CSize -> Ptr CSize -> IO ArchiveError

foreign import ccall unsafe archive_write_header :: Ptr Archive -> Ptr ArchiveEntry -> IO ArchiveError
foreign import ccall unsafe archive_write_data :: Ptr Archive -> Ptr a -> CSize -> IO CSize

foreign import ccall unsafe archive_write_data_block :: Ptr Archive -> Ptr a -> CSize -> Int64 -> IO ArchiveError

foreign import ccall unsafe archive_write_finish_entry :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_close :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_fail :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_free :: Ptr Archive -> IO ArchiveError

foreign import ccall unsafe archive_write_set_format_option :: Ptr Archive -> CString -> CString -> CString -> IO ArchiveError
foreign import ccall unsafe archive_write_set_filter_option :: Ptr Archive -> CString -> CString -> CString -> IO ArchiveError
foreign import ccall unsafe archive_write_set_option :: Ptr Archive -> CString -> CString -> CString -> IO ArchiveError
foreign import ccall unsafe archive_write_set_options :: Ptr Archive -> CString -> IO ArchiveError

foreign import ccall unsafe archive_write_set_passphrase :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall unsafe archive_write_set_passphrase_callback :: Ptr Archive -> Ptr a -> FunPtr (ArchivePassphraseCallback a) -> IO ArchiveError

foreign import ccall unsafe archive_write_disk_new :: IO (Ptr Archive)
foreign import ccall unsafe archive_write_disk_set_skip_file :: Ptr Archive -> Int64 -> Int64 -> IO ArchiveError
foreign import ccall unsafe archive_write_disk_set_options :: Ptr Archive -> Flags -> IO ArchiveError

foreign import ccall unsafe archive_write_disk_set_standard_lookup :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_disk_set_group_lookup :: Ptr Archive -> Ptr a -> FunPtr (Ptr a -> CString -> Int64 -> IO Int64) -> FunPtr (Ptr a -> IO ()) -> IO ArchiveError
foreign import ccall unsafe archive_write_disk_set_user_lookup :: Ptr Archive -> Ptr a -> FunPtr (Ptr a -> CString -> Int64 -> IO Int64) -> FunPtr (Ptr a -> IO ()) -> IO ArchiveError
foreign import ccall unsafe archive_write_disk_gid :: Ptr Archive -> CString -> Int64 -> IO Int64
foreign import ccall unsafe archive_write_disk_uid :: Ptr Archive -> CString -> Int64 -> IO Int64

foreign import ccall unsafe archive_read_disk_new :: IO (Ptr Archive)
foreign import ccall unsafe archive_read_disk_set_symlink_logical :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_disk_set_symlink_physical :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_disk_set_symlink_hybrid :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_disk_entry_from_file :: Ptr Archive -> Ptr ArchiveEntry -> Fd -> Ptr Stat -> IO ArchiveError
foreign import ccall unsafe archive_read_disk_gname :: Ptr Archive -> Int64 -> IO CString
foreign import ccall unsafe archive_read_disk_uname :: Ptr Archive -> Int64 -> IO CString
foreign import ccall unsafe archive_read_disk_set_standard_lookup :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_disk_set_gname_lookup :: Ptr Archive -> Ptr a -> FunPtr (Ptr a -> Int64 -> IO CString) -> FunPtr (Ptr a -> IO ()) -> IO ArchiveError
foreign import ccall unsafe archive_read_disk_set_uname_lookup :: Ptr Archive -> Ptr a -> FunPtr (Ptr a -> Int64 -> IO CString) -> FunPtr (Ptr a -> IO ()) -> IO ArchiveError
foreign import ccall unsafe archive_read_disk_open :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall unsafe archive_read_disk_open_w :: Ptr Archive -> CWString -> IO ArchiveError
foreign import ccall unsafe archive_read_disk_descend :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_disk_can_descend :: Ptr Archive -> IO CInt
foreign import ccall unsafe archive_read_disk_current_filesystem :: Ptr Archive -> IO CInt
foreign import ccall unsafe archive_read_disk_current_filesystem_is_synthetic :: Ptr Archive -> IO CInt
foreign import ccall unsafe archive_read_disk_current_filesystem_is_remote :: Ptr Archive -> IO CInt
foreign import ccall unsafe archive_read_disk_set_atime_restored :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_disk_set_behavior :: Ptr Archive -> ReadDiskFlags -> IO ArchiveError

foreign import ccall unsafe archive_read_disk_set_matching :: Ptr Archive -> Ptr Archive -> FunPtr (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO ()) -> Ptr a -> IO ArchiveError
foreign import ccall unsafe archive_read_disk_set_metadata_filter_callback :: Ptr Archive -> FunPtr (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO CInt) -> Ptr a -> IO ArchiveError

foreign import ccall unsafe archive_free :: Ptr Archive -> IO ArchiveError

foreign import ccall unsafe archive_filter_count :: Ptr Archive -> IO CInt
foreign import ccall unsafe archive_filter_bytes :: Ptr Archive -> CInt -> Int64
foreign import ccall unsafe archive_filter_code :: Ptr Archive -> CInt -> IO Int
foreign import ccall unsafe archive_filter_name :: Ptr Archive -> CInt -> IO CString

foreign import ccall unsafe archive_errno :: Ptr Archive -> IO CInt
foreign import ccall unsafe archive_error_string :: Ptr Archive -> IO CString
foreign import ccall unsafe archive_format_name :: Ptr Archive -> IO CString
foreign import ccall unsafe archive_format :: Ptr Archive -> IO ArchiveFormat
foreign import ccall unsafe archive_clear_error :: Ptr Archive -> IO ()
foreign import ccall unsafe archive_set_error :: Ptr Archive -> CInt -> CString -> IO () -- TODO: variadic lol
foreign import ccall unsafe archive_copy_error :: Ptr Archive -> Ptr Archive -> IO ()
foreign import ccall unsafe archive_file_count :: Ptr Archive -> IO CInt

foreign import ccall unsafe archive_match :: Ptr Archive
foreign import ccall unsafe archive_match_free :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_match_excluded :: Ptr Archive -> IO CInt
foreign import ccall unsafe archive_match_path_excluded :: Ptr Archive -> Ptr ArchiveEntry -> IO CInt
foreign import ccall unsafe archive_match_exclude_pattern :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall unsafe archive_match_exclude_pattern_w :: Ptr Archive -> CWString -> IO ArchiveError
foreign import ccall unsafe archive_match_exclude_pattern_from_file :: Ptr Archive -> CString -> CInt -> IO ArchiveError
foreign import ccall unsafe archive_match_exclude_pattern_from_file_w :: Ptr Archive -> CWString -> CInt -> IO ArchiveError
foreign import ccall unsafe archive_match_include_pattern :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall unsafe archive_match_include_pattern_w :: Ptr Archive -> CWString -> IO ArchiveError
foreign import ccall unsafe archive_match_include_pattern_from_file :: Ptr Archive -> CString -> CInt -> IO ArchiveError
foreign import ccall unsafe archive_match_include_pattern_from_file_w :: Ptr Archive -> CString -> CInt -> IO ArchiveError
foreign import ccall unsafe archive_match_path_unmatched_inclusions :: Ptr Archive -> IO CInt
foreign import ccall unsafe archive_match_path_unmatched_inclusions_next :: Ptr Archive -> Ptr CString -> IO ArchiveError
foreign import ccall unsafe archive_match_path_unmatched_inclusions_next_w :: Ptr Archive -> Ptr CWString -> IO ArchiveError
foreign import ccall unsafe archive_match_time_excluded :: Ptr Archive -> Ptr ArchiveEntry -> IO CInt
foreign import ccall unsafe archive_match_include_time :: Ptr Archive -> TimeFlag -> CTime -> CLong -> IO ArchiveError
foreign import ccall unsafe archive_match_include_date :: Ptr Archive -> TimeFlag -> CString -> IO ArchiveError
foreign import ccall unsafe archive_match_include_date_w :: Ptr Archive -> TimeFlag -> CWString -> IO ArchiveError
foreign import ccall unsafe archive_match_include_file_time :: Ptr Archive -> TimeFlag -> CString -> IO ArchiveError
foreign import ccall unsafe archive_match_include_file_time_w :: Ptr Archive -> TimeFlag -> CWString -> IO ArchiveError
foreign import ccall unsafe archive_match_exclude_entry :: Ptr Archive -> TimeFlag -> Ptr ArchiveEntry -> IO ArchiveError
foreign import ccall unsafe archive_match_owner_excluded :: Ptr Archive -> Ptr ArchiveEntry -> IO CInt
foreign import ccall unsafe archive_match_include_gid :: Ptr Archive -> Id -> IO ArchiveError
foreign import ccall unsafe archive_match_include_uid :: Ptr Archive -> Id -> IO ArchiveError
foreign import ccall unsafe archive_match_include_uname :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall unsafe archive_match_include_uname_w :: Ptr Archive -> CWString -> IO ArchiveError
foreign import ccall unsafe archive_match_include_gname :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall unsafe archive_match_include_gname_w :: Ptr Archive -> CWString -> IO ArchiveError

#include <archive.h>

archiveVersionNumber :: Int
archiveVersionNumber = {# const ARCHIVE_VERSION_NUMBER #}

archiveVersionOnlyString :: String
archiveVersionOnlyString = {# const ARCHIVE_VERSION_ONLY_STRING #}

archiveVersionString :: String
archiveVersionString = {# const ARCHIVE_VERSION_STRING #}

-- TODO: make ArchiveError a sum type
archiveOk :: ArchiveError
archiveOk = ArchiveError {# const ARCHIVE_OK #}

archiveEOF :: ArchiveError
archiveEOF = ArchiveError {# const ARCHIVE_EOF #}

archiveRetry :: ArchiveError
archiveRetry = ArchiveError ({# const ARCHIVE_RETRY #})

archiveWarn :: ArchiveError
archiveWarn = ArchiveError ({# const ARCHIVE_WARN #})

archiveFailed :: ArchiveError
archiveFailed = ArchiveError ({# const ARCHIVE_FAILED #})

archiveFatal :: ArchiveError
archiveFatal = ArchiveError ({# const ARCHIVE_FATAL #})

-- Archive filter
archiveFilterNone :: ArchiveFilter
archiveFilterNone = ArchiveFilter {# const ARCHIVE_FILTER_NONE #}

archiveFilterGzip :: ArchiveFilter
archiveFilterGzip = ArchiveFilter {# const ARCHIVE_FILTER_GZIP #}

archiveFilterBzip2 :: ArchiveFilter
archiveFilterBzip2 = ArchiveFilter {# const ARCHIVE_FILTER_BZIP2 #}

archiveFilterCompress :: ArchiveFilter
archiveFilterCompress = ArchiveFilter {# const ARCHIVE_FILTER_COMPRESS #}

archiveFilterProgram :: ArchiveFilter
archiveFilterProgram = ArchiveFilter {# const ARCHIVE_FILTER_PROGRAM #}

archiveFilterLzma :: ArchiveFilter
archiveFilterLzma = ArchiveFilter {# const ARCHIVE_FILTER_LZMA #}

archiveFilterXz :: ArchiveFilter
archiveFilterXz = ArchiveFilter {# const ARCHIVE_FILTER_XZ #}

archiveFilterUu :: ArchiveFilter
archiveFilterUu = ArchiveFilter {# const ARCHIVE_FILTER_UU #}

archiveFilterRpm :: ArchiveFilter
archiveFilterRpm = ArchiveFilter {# const ARCHIVE_FILTER_RPM #}

archiveFilterLzip :: ArchiveFilter
archiveFilterLzip = ArchiveFilter {# const ARCHIVE_FILTER_LZIP #}

archiveFilterLrzip :: ArchiveFilter
archiveFilterLrzip = ArchiveFilter {# const ARCHIVE_FILTER_LRZIP #}

archiveFilterLzop :: ArchiveFilter
archiveFilterLzop = ArchiveFilter {# const ARCHIVE_FILTER_LZOP #}

archiveFilterGrzip :: ArchiveFilter
archiveFilterGrzip = ArchiveFilter {# const ARCHIVE_FILTER_GRZIP #}

archiveFilterLz4 :: ArchiveFilter
archiveFilterLz4 = ArchiveFilter {# const ARCHIVE_FILTER_LZ4 #}

-- Extraction flags
archiveExtractOwner :: Flags
archiveExtractOwner = Flags {# const ARCHIVE_EXTRACT_OWNER #}

archiveExtractPerm :: Flags
archiveExtractPerm = Flags {# const ARCHIVE_EXTRACT_PERM #}

archiveExtractTime :: Flags
archiveExtractTime = Flags {# const ARCHIVE_EXTRACT_TIME #}

archiveExtractNoOverwrite :: Flags
archiveExtractNoOverwrite = Flags {# const ARCHIVE_EXTRACT_NO_OVERWRITE #}

archiveExtractUnlink :: Flags
archiveExtractUnlink = Flags {# const ARCHIVE_EXTRACT_UNLINK #}

archiveExtractACL :: Flags
archiveExtractACL = Flags {# const ARCHIVE_EXTRACT_ACL #}

archiveExtractFFlags :: Flags
archiveExtractFFlags = Flags {# const ARCHIVE_EXTRACT_FFLAGS #}

archiveExtractXattr :: Flags
archiveExtractXattr = Flags {# const ARCHIVE_EXTRACT_XATTR #}

archiveExtractSecureSymlinks :: Flags
archiveExtractSecureSymlinks = Flags {# const ARCHIVE_EXTRACT_SECURE_SYMLINKS #}

archiveExtractSecureNoDotDot :: Flags
archiveExtractSecureNoDotDot = Flags {# const ARCHIVE_EXTRACT_SECURE_NODOTDOT #}

archiveExtractNoAutodir :: Flags
archiveExtractNoAutodir = Flags {# const ARCHIVE_EXTRACT_NO_AUTODIR #}

-- archiveExtractNoOverwriteNewer :: Flags
-- archiveExtractNoOverwriteNewer = Flags {# const ARCHIVE_NO_OVERWRITE_NEWER #}

archiveExtractSparse :: Flags
archiveExtractSparse = Flags {# const ARCHIVE_EXTRACT_SPARSE #}

archiveExtractMacMetadata :: Flags
archiveExtractMacMetadata = Flags {# const ARCHIVE_EXTRACT_MAC_METADATA #}

archiveExtractNoHfsCompression :: Flags
archiveExtractNoHfsCompression = Flags {# const ARCHIVE_EXTRACT_NO_HFS_COMPRESSION #}

archiveExtractHfsCompressionForced :: Flags
archiveExtractHfsCompressionForced = Flags {# const ARCHIVE_EXTRACT_HFS_COMPRESSION_FORCED #}

archiveExtractSecureNoAbsolutePaths :: Flags
archiveExtractSecureNoAbsolutePaths = Flags {# const ARCHIVE_EXTRACT_SECURE_NOABSOLUTEPATHS #}

archiveExtractClearNoChangeFFlags :: Flags
archiveExtractClearNoChangeFFlags = Flags {# const ARCHIVE_EXTRACT_CLEAR_NOCHANGE_FFLAGS #}

archiveFormatCpio :: ArchiveFormat
archiveFormatCpio = ArchiveFormat {# const ARCHIVE_FORMAT_CPIO #}

archiveFormatShar :: ArchiveFormat
archiveFormatShar = ArchiveFormat {# const ARCHIVE_FORMAT_SHAR #}

archiveFormatTar :: ArchiveFormat
archiveFormatTar = ArchiveFormat {# const ARCHIVE_FORMAT_TAR #}

archiveFormatIso9660 :: ArchiveFormat
archiveFormatIso9660 = ArchiveFormat {# const ARCHIVE_FORMAT_ISO9660 #}

archiveFormatZip :: ArchiveFormat
archiveFormatZip = ArchiveFormat {# const ARCHIVE_FORMAT_ZIP #}

archiveFormatEmpty :: ArchiveFormat
archiveFormatEmpty = ArchiveFormat {# const ARCHIVE_FORMAT_EMPTY #}

archiveFormatAr :: ArchiveFormat
archiveFormatAr = ArchiveFormat {# const ARCHIVE_FORMAT_AR #}

archiveFormatMtree :: ArchiveFormat
archiveFormatMtree = ArchiveFormat {# const ARCHIVE_FORMAT_MTREE #}

archiveFormatRaw :: ArchiveFormat
archiveFormatRaw = ArchiveFormat {# const ARCHIVE_FORMAT_RAW #}

archiveFormatXar :: ArchiveFormat
archiveFormatXar = ArchiveFormat {# const ARCHIVE_FORMAT_XAR #}

archiveFormatLha :: ArchiveFormat
archiveFormatLha = ArchiveFormat {# const ARCHIVE_FORMAT_LHA #}

archiveFormatCab :: ArchiveFormat
archiveFormatCab = ArchiveFormat {# const ARCHIVE_FORMAT_CAB #}

archiveFormatRar :: ArchiveFormat
archiveFormatRar = ArchiveFormat {# const ARCHIVE_FORMAT_RAR #}

archiveFormat7zip :: ArchiveFormat
archiveFormat7zip = ArchiveFormat {# const ARCHIVE_FORMAT_7ZIP #}

archiveFormatWarc :: ArchiveFormat
archiveFormatWarc = ArchiveFormat {# const ARCHIVE_FORMAT_WARC #}

archiveReadHasEncryptedEntries :: Ptr Archive -> IO ArchiveEncryption
archiveReadHasEncryptedEntries = fmap encryptionResult . archive_read_has_encrypted_entries

encryptionResult :: CInt -> ArchiveEncryption
encryptionResult 0                                                        = NoEncryption
encryptionResult 1                                                        = HasEncryption
encryptionResult ({# const ARCHIVE_READ_FORMAT_ENCRYPTION_UNSUPPORTED #}) = EncryptionUnsupported
encryptionResult ({# const ARCHIVE_READ_FORMAT_ENCRYPTION_DONT_KNOW #})   = EncryptionUnknown
encryptionResult _                                                        = error "Should not happen."

archiveReadFormatCapsNone :: ArchiveCapabilities
archiveReadFormatCapsNone = ArchiveCapabilities {# const ARCHIVE_READ_FORMAT_CAPS_NONE #}

(<<) :: Bits a => a -> Int -> a
m << n = m `shift` n

archiveReadFormatCapsEncryptData :: ArchiveCapabilities
archiveReadFormatCapsEncryptData = ArchiveCapabilities ({# const ARCHIVE_READ_FORMAT_CAPS_ENCRYPT_DATA #})

archiveReadFormatCapsEncryptMetadata :: ArchiveCapabilities
archiveReadFormatCapsEncryptMetadata = ArchiveCapabilities ({# const ARCHIVE_READ_FORMAT_CAPS_ENCRYPT_DATA #})

archiveReadDiskCanDescend :: Ptr Archive -> IO Bool
archiveReadDiskCanDescend = fmap intToBool . archive_read_disk_can_descend

archiveReadDiskCurrentFilesystemIsSynthetic :: Ptr Archive -> IO Bool
archiveReadDiskCurrentFilesystemIsSynthetic = fmap intToBool . archive_read_disk_current_filesystem_is_synthetic

archiveReadDiskCurrentFilesystemIsRemote :: Ptr Archive -> IO Bool
archiveReadDiskCurrentFilesystemIsRemote = fmap intToBool . archive_read_disk_current_filesystem_is_remote

archiveReadDiskRestoreATime :: ReadDiskFlags
archiveReadDiskRestoreATime = ReadDiskFlags {# const ARCHIVE_READDISK_RESTORE_ATIME #}

archiveReadDiskHonorNoDump :: ReadDiskFlags
archiveReadDiskHonorNoDump = ReadDiskFlags {# const ARCHIVE_READDISK_HONOR_NODUMP #}

archiveReadDiskMacCopyFile :: ReadDiskFlags
archiveReadDiskMacCopyFile = ReadDiskFlags {# const ARCHIVE_READDISK_MAC_COPYFILE #}

archiveReadDiskNoTraverseMounts :: ReadDiskFlags
archiveReadDiskNoTraverseMounts = ReadDiskFlags {# const ARCHIVE_READDISK_NO_TRAVERSE_MOUNTS #}

archiveReadDiskNoXattr :: ReadDiskFlags
archiveReadDiskNoXattr = ReadDiskFlags {# const ARCHIVE_READDISK_NO_XATTR #}

-- archiveReadDiskNoAcl :: ReadDiskFlags
-- archiveReadDiskNoAcl = ReadDiskFlags {# const ARCHIVE_READDISK_NO_ACL #}

-- archiveReadDiskNoFFlags :: ReadDiskFlags
-- archiveReadDiskNoFFlags = ReadDiskFlags {# const ARCHIVE_READDISK_NO_FFLAGS #}

archiveMatchExcluded :: Ptr Archive -> IO Bool
archiveMatchExcluded = fmap intToBool . archive_match_excluded

archiveMatchPathExcluded :: Ptr Archive -> Ptr ArchiveEntry -> IO Bool
archiveMatchPathExcluded = fmap intToBool .* archive_match_path_excluded

archiveMatchExcludePatternFromFile :: Ptr Archive -> CString -> Bool -> IO ArchiveError
archiveMatchExcludePatternFromFile a str b = archive_match_exclude_pattern_from_file a str (boolToInt b)

archiveMatchExcludePatternFromFileW :: Ptr Archive -> CWString -> Bool -> IO ArchiveError
archiveMatchExcludePatternFromFileW a str b = archive_match_exclude_pattern_from_file_w a str (boolToInt b)

archiveMatchIncludePatternFromFile :: Ptr Archive -> CString -> Bool -> IO ArchiveError
archiveMatchIncludePatternFromFile a str b = archive_match_include_pattern_from_file a str (boolToInt b)

archiveMatchIncludePatternFromFileW :: Ptr Archive -> CString -> Bool -> IO ArchiveError
archiveMatchIncludePatternFromFileW a str b = archive_match_include_pattern_from_file_w a str (boolToInt b)

archiveMatchTimeExcluded :: Ptr Archive -> Ptr ArchiveEntry -> IO Bool
archiveMatchTimeExcluded = fmap intToBool .* archive_match_time_excluded

archiveMatchMTime :: TimeFlag
archiveMatchMTime = TimeFlag {# const ARCHIVE_MATCH_MTIME #}

archiveMatchCTime :: TimeFlag
archiveMatchCTime = TimeFlag {# const ARCHIVE_MATCH_CTIME #}

archiveMatchNewer :: TimeFlag
archiveMatchNewer = TimeFlag {# const ARCHIVE_MATCH_NEWER #}

archiveMatchOlder :: TimeFlag
archiveMatchOlder = TimeFlag {# const ARCHIVE_MATCH_OLDER #}

archiveMatchEqual :: TimeFlag
archiveMatchEqual = TimeFlag {# const ARCHIVE_MATCH_EQUAL #}

archiveMatchOwnerExcluded :: Ptr Archive -> Ptr ArchiveEntry -> IO Bool
archiveMatchOwnerExcluded = fmap intToBool .* archive_match_owner_excluded
