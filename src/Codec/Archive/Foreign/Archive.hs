-- | This module corresponds to @archive.h@
--
-- Functions in this module are stateful and hence take place in the 'IO'
-- monad.
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
                                     , archive_match_new
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
                                     -- * Direct bindings (version\/filter\/miscellaneous)
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
                                     -- * Higher-level function equivalents
                                     , archiveReadNextHeader
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
                                     , ArchiveOpenCallbackRaw
                                     , ArchiveCloseCallbackRaw
                                     , ArchiveSwitchCallbackRaw
                                     , ArchivePassphraseCallback
                                     -- * Callback constructors
                                     , noOpenCallback
                                     , mkReadCallback
                                     , mkSkipCallback
                                     , mkSeekCallback
                                     , mkWriteCallback
                                     , mkPassphraseCallback
                                     , mkOpenCallback
                                     , mkCloseCallback
                                     , mkSwitchCallback
                                     , mkWriteLookup
                                     , mkReadLookup
                                     , mkCleanup
                                     , mkMatch
                                     , mkFilter
                                     ) where

import           Codec.Archive.Foreign.Archive.Macros
import           Codec.Archive.Foreign.Archive.Raw
import           Codec.Archive.Foreign.Common
import           Codec.Archive.Types
import           Control.Composition                  ((.*), (.**))
import           Data.Int                             (Int64)
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr

-- destructors: use "dynamic" instead of "wrapper" (but we don't want that)
-- callbacks
foreign import ccall "wrapper" mkReadCallback :: ArchiveReadCallback a b -> IO (FunPtr (ArchiveReadCallback a b))
foreign import ccall "wrapper" mkSkipCallback :: ArchiveSkipCallback a -> IO (FunPtr (ArchiveSkipCallback a))
foreign import ccall "wrapper" mkSeekCallback :: ArchiveSeekCallback a -> IO (FunPtr (ArchiveSeekCallback a))
foreign import ccall "wrapper" mkWriteCallback :: ArchiveWriteCallback a b -> IO (FunPtr (ArchiveWriteCallback a b))
foreign import ccall "wrapper" mkOpenCallbackRaw :: ArchiveOpenCallbackRaw a -> IO (FunPtr (ArchiveOpenCallbackRaw a))
foreign import ccall "wrapper" mkCloseCallbackRaw :: ArchiveCloseCallbackRaw a -> IO (FunPtr (ArchiveCloseCallbackRaw a))
foreign import ccall "wrapper" mkSwitchCallbackRaw :: ArchiveSwitchCallbackRaw a b -> IO (FunPtr (ArchiveSwitchCallbackRaw a b))
foreign import ccall "wrapper" mkPassphraseCallback :: ArchivePassphraseCallback a -> IO (FunPtr (ArchivePassphraseCallback a))

-- | Don't use an open callback. This is the recommended argument to 'archive_open_read'
noOpenCallback :: FunPtr (ArchiveOpenCallbackRaw a)
noOpenCallback = castPtrToFunPtr nullPtr

foreign import ccall "wrapper" mkWriteLookup :: (Ptr a -> CString -> Int64 -> IO Int64) -> IO (FunPtr (Ptr a -> CString -> Int64 -> IO Int64))
foreign import ccall "wrapper" mkReadLookup :: (Ptr a -> Int64 -> IO CString) -> IO (FunPtr (Ptr a -> Int64 -> IO CString))
foreign import ccall "wrapper" mkCleanup :: (Ptr a -> IO ()) -> IO (FunPtr (Ptr a -> IO ()))

foreign import ccall "wrapper" mkMatch :: (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO ()) -> IO (FunPtr (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO ()))
foreign import ccall "wrapper" preMkFilter :: (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO CInt) -> IO (FunPtr (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO CInt))

mkOpenCallback :: ArchiveOpenCallback a -> IO (FunPtr (ArchiveOpenCallbackRaw a))
mkOpenCallback f = let f' = fmap resultToErr .* f in mkOpenCallbackRaw f'

mkCloseCallback :: ArchiveCloseCallback a -> IO (FunPtr (ArchiveCloseCallbackRaw a))
mkCloseCallback f = let f' = fmap resultToErr .* f in mkCloseCallbackRaw f'

mkSwitchCallback :: ArchiveSwitchCallback a b -> IO (FunPtr (ArchiveSwitchCallbackRaw a b))
mkSwitchCallback f = let f' = fmap resultToErr .** f in mkSwitchCallbackRaw f'

mkFilter :: (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO Bool) -> IO (FunPtr (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO CInt))
mkFilter f = let f' = fmap boolToInt .** f in preMkFilter f'

boolToInt :: Integral a => Bool -> a
boolToInt False = 0
boolToInt True  = 1

archiveReadDiskCanDescend :: Ptr Archive -> IO Bool
archiveReadDiskCanDescend = fmap intToBool . archive_read_disk_can_descend

archiveReadDiskCurrentFilesystemIsSynthetic :: Ptr Archive -> IO Bool
archiveReadDiskCurrentFilesystemIsSynthetic = fmap intToBool . archive_read_disk_current_filesystem_is_synthetic

archiveReadDiskCurrentFilesystemIsRemote :: Ptr Archive -> IO Bool
archiveReadDiskCurrentFilesystemIsRemote = fmap intToBool . archive_read_disk_current_filesystem_is_remote

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

archiveReadHasEncryptedEntries :: Ptr Archive -> IO ArchiveEncryption
archiveReadHasEncryptedEntries = fmap encryptionResult . archive_read_has_encrypted_entries

archiveMatchOwnerExcluded :: Ptr Archive -> Ptr ArchiveEntry -> IO Bool
archiveMatchOwnerExcluded = fmap intToBool .* archive_match_owner_excluded

archiveReadNextHeader :: Ptr Archive -> Ptr (Ptr ArchiveEntry) -> IO ArchiveResult
archiveReadNextHeader = fmap errorRes .* archive_read_next_header
