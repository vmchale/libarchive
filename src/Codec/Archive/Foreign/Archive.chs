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
                                     , archive_write_open_filename
                                     , archive_free
                                     -- * Direct bindings (version/miscellaneous)
                                     , archive_version_number
                                     , archive_version_string
                                     , archive_version_details
                                     -- * Version macros
                                     , archiveVersionNumber
                                     , archiveVersionOnlyString
                                     , archiveVersionString
                                     -- * Header read macros
                                     , archiveOk
                                     , archiveEOF
                                     , archiveRetry
                                     , archiveWarn
                                     , archiveFailed
                                     , archiveFatal
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
                                     -- * Abstract types
                                     , Archive
                                     -- * Lower-level API types
                                     , ArchiveError
                                     , ExtractFlags
                                     , ArchiveFilter
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
                                     ) where

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
foreign import ccall unsafe archive_read_add_passphrase :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall unsafe archive_read_set_passphrase_callback :: Ptr Archive -> Ptr a -> FunPtr (ArchivePassphraseCallback a) -> IO ArchiveError
foreign import ccall unsafe archive_read_extract :: Ptr Archive -> Ptr ArchiveEntry -> ExtractFlags -> IO ArchiveError
foreign import ccall unsafe archive_read_extract2 :: Ptr Archive -> Ptr ArchiveEntry -> Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_extract_set_progress_callback :: Ptr Archive -> (FunPtr (Ptr a -> IO ())) -> Ptr a -> IO ()
foreign import ccall unsafe archive_read_extract_set_skip_file :: Ptr Archive -> Int64 -> Int64 -> IO ()
foreign import ccall unsafe archive_read_close :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_free :: Ptr Archive -> IO ArchiveError

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

foreign import ccall unsafe archive_write_data :: Ptr Archive -> CString -> CSize -> IO CSize
foreign import ccall unsafe archive_write_set_format_pax_restricted :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_write_header :: Ptr Archive -> Ptr ArchiveEntry -> IO ArchiveError
foreign import ccall unsafe archive_write_open_filename :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall unsafe archive_write_free :: Ptr Archive -> IO ArchiveError

foreign import ccall unsafe archive_free :: Ptr Archive -> IO ArchiveError

#include <archive.h>

archiveVersionNumber :: Int
archiveVersionNumber = {# const ARCHIVE_VERSION_NUMBER #}

archiveVersionOnlyString :: String
archiveVersionOnlyString = {# const ARCHIVE_VERSION_ONLY_STRING #}

archiveVersionString :: String
archiveVersionString = {# const ARCHIVE_VERSION_STRING #}

-- TODO: make ArchiveError a sum type
archiveOk :: ArchiveError
archiveOk = {# const ARCHIVE_OK #}

archiveEOF :: ArchiveError
archiveEOF = {# const ARCHIVE_EOF #}

archiveRetry :: ArchiveError
archiveRetry = {# const ARCHIVE_RETRY #}

archiveWarn :: ArchiveError
archiveWarn = {# const ARCHIVE_WARN #}

archiveFailed :: ArchiveError
archiveFailed = {# const ARCHIVE_FAILED #}

archiveFatal :: ArchiveError
archiveFatal = {# const ARCHIVE_FATAL #}

-- Archive filter
archiveFilterNone :: ArchiveFilter
archiveFilterNone = {# const ARCHIVE_FILTER_NONE #}

archiveFilterGzip :: ArchiveFilter
archiveFilterGzip = {# const ARCHIVE_FILTER_GZIP #}

archiveFilterBzip2 :: ArchiveFilter
archiveFilterBzip2 = {# const ARCHIVE_FILTER_BZIP2 #}

archiveFilterCompress :: ArchiveFilter
archiveFilterCompress = {# const ARCHIVE_FILTER_COMPRESS #}

archiveFilterProgram :: ArchiveFilter
archiveFilterProgram = {# const ARCHIVE_FILTER_PROGRAM #}

archiveFilterLzma :: ArchiveFilter
archiveFilterLzma = {# const ARCHIVE_FILTER_LZMA #}

archiveFilterXz :: ArchiveFilter
archiveFilterXz = {# const ARCHIVE_FILTER_XZ #}

archiveFilterUu :: ArchiveFilter
archiveFilterUu = {# const ARCHIVE_FILTER_UU #}

archiveFilterRpm :: ArchiveFilter
archiveFilterRpm = {# const ARCHIVE_FILTER_RPM #}

archiveFilterLzip :: ArchiveFilter
archiveFilterLzip = {# const ARCHIVE_FILTER_LZIP #}

archiveFilterLrzip :: ArchiveFilter
archiveFilterLrzip = {# const ARCHIVE_FILTER_LRZIP #}

archiveFilterLzop :: ArchiveFilter
archiveFilterLzop = {# const ARCHIVE_FILTER_LZOP #}

archiveFilterGrzip :: ArchiveFilter
archiveFilterGrzip = {# const ARCHIVE_FILTER_GRZIP #}

archiveFilterLz4 :: ArchiveFilter
archiveFilterLz4 = {# const ARCHIVE_FILTER_LZ4 #}

-- Extraction flags
archiveExtractOwner :: ExtractFlags
archiveExtractOwner = {# const ARCHIVE_EXTRACT_OWNER #}

archiveExtractPerm :: ExtractFlags
archiveExtractPerm = {# const ARCHIVE_EXTRACT_PERM #}

archiveExtractTime :: ExtractFlags
archiveExtractTime = {# const ARCHIVE_EXTRACT_TIME #}

archiveExtractNoOverwrite :: ExtractFlags
archiveExtractNoOverwrite = {# const ARCHIVE_EXTRACT_NO_OVERWRITE #}

archiveExtractUnlink :: ExtractFlags
archiveExtractUnlink = {# const ARCHIVE_EXTRACT_UNLINK #}

archiveExtractACL :: ExtractFlags
archiveExtractACL = {# const ARCHIVE_EXTRACT_ACL #}

archiveExtractFFlags :: ExtractFlags
archiveExtractFFlags = {# const ARCHIVE_EXTRACT_FFLAGS #}

archiveExtractXattr :: ExtractFlags
archiveExtractXattr = {# const ARCHIVE_EXTRACT_XATTR #}

archiveExtractSecureSymlinks :: ExtractFlags
archiveExtractSecureSymlinks = {# const ARCHIVE_EXTRACT_SECURE_SYMLINKS #}

archiveExtractSecureNoDotDot :: ExtractFlags
archiveExtractSecureNoDotDot = {# const ARCHIVE_EXTRACT_SECURE_NODOTDOT #}

archiveExtractNoAutodir :: ExtractFlags
archiveExtractNoAutodir = {# const ARCHIVE_EXTRACT_NO_AUTODIR #}

-- archiveExtractNoOverwriteNewer :: ExtractFlags
-- archiveExtractNoOverwriteNewer = {# const ARCHIVE_NO_OVERWRITE_NEWER #}

archiveExtractSparse :: ExtractFlags
archiveExtractSparse = {# const ARCHIVE_EXTRACT_SPARSE #}

archiveExtractMacMetadata :: ExtractFlags
archiveExtractMacMetadata = {# const ARCHIVE_EXTRACT_MAC_METADATA #}

archiveExtractNoHfsCompression :: ExtractFlags
archiveExtractNoHfsCompression = {# const ARCHIVE_EXTRACT_NO_HFS_COMPRESSION #}

archiveExtractHfsCompressionForced :: ExtractFlags
archiveExtractHfsCompressionForced = {# const ARCHIVE_EXTRACT_HFS_COMPRESSION_FORCED #}

archiveExtractSecureNoAbsolutePaths :: ExtractFlags
archiveExtractSecureNoAbsolutePaths = {# const ARCHIVE_EXTRACT_SECURE_NOABSOLUTEPATHS #}

archiveExtractClearNoChangeFFlags :: ExtractFlags
archiveExtractClearNoChangeFFlags = {# const ARCHIVE_EXTRACT_CLEAR_NOCHANGE_FFLAGS #}

archiveFormatCpio :: ArchiveFormat
archiveFormatCpio = {# const ARCHIVE_FORMAT_CPIO #}

archiveFormatShar :: ArchiveFormat
archiveFormatShar = {# const ARCHIVE_FORMAT_SHAR #}

archiveFormatTar :: ArchiveFormat
archiveFormatTar = {# const ARCHIVE_FORMAT_TAR #}

archiveFormatIso9660 :: ArchiveFormat
archiveFormatIso9660 = {# const ARCHIVE_FORMAT_ISO9660 #}

archiveFormatZip :: ArchiveFormat
archiveFormatZip = {# const ARCHIVE_FORMAT_ZIP #}

archiveFormatEmpty :: ArchiveFormat
archiveFormatEmpty = {# const ARCHIVE_FORMAT_EMPTY #}

archiveFormatAr :: ArchiveFormat
archiveFormatAr = {# const ARCHIVE_FORMAT_AR #}

archiveFormatMtree :: ArchiveFormat
archiveFormatMtree = {# const ARCHIVE_FORMAT_MTREE #}

archiveFormatRaw :: ArchiveFormat
archiveFormatRaw = {# const ARCHIVE_FORMAT_RAW #}

archiveFormatXar :: ArchiveFormat
archiveFormatXar = {# const ARCHIVE_FORMAT_XAR #}

archiveFormatLha :: ArchiveFormat
archiveFormatLha = {# const ARCHIVE_FORMAT_LHA #}

archiveFormatCab :: ArchiveFormat
archiveFormatCab = {# const ARCHIVE_FORMAT_CAB #}

archiveFormatRar :: ArchiveFormat
archiveFormatRar = {# const ARCHIVE_FORMAT_RAR #}

archiveFormat7zip :: ArchiveFormat
archiveFormat7zip = {# const ARCHIVE_FORMAT_7ZIP #}

archiveFormatWarc :: ArchiveFormat
archiveFormatWarc = {# const ARCHIVE_FORMAT_WARC #}

archiveReadHasEncryptedEntries :: Ptr Archive -> IO ArchiveEncryption
archiveReadHasEncryptedEntries = fmap encryptionResult . archive_read_has_encrypted_entries

encryptionResult :: CInt -> ArchiveEncryption
encryptionResult 0                                                        = NoEncryption
encryptionResult 1                                                        = HasEncryption
encryptionResult ({# const ARCHIVE_READ_FORMAT_ENCRYPTION_UNSUPPORTED #}) = EncryptionUnsupported
encryptionResult ({# const ARCHIVE_READ_FORMAT_ENCRYPTION_DONT_KNOW #})   = EncryptionUnknown
encryptionResult _                                                        = error "Should not happen."

archiveReadFormatCapsNone :: ArchiveCapabilities
archiveReadFormatCapsNone = {# const ARCHIVE_READ_FORMAT_CAPS_NONE #}

(<<) :: Bits a => a -> Int -> a
m << n = m `shift` n

archiveReadFormatCapsEncryptData :: ArchiveCapabilities
archiveReadFormatCapsEncryptData = {# const ARCHIVE_READ_FORMAT_CAPS_ENCRYPT_DATA #}

archiveReadFormatCapsEncryptMetadata :: ArchiveCapabilities
archiveReadFormatCapsEncryptMetadata = {# const ARCHIVE_READ_FORMAT_CAPS_ENCRYPT_DATA #}
