-- | Module corresponding loosely to @archive.h@
module Codec.Archive.Foreign.Archive ( -- * Direct bindings (read)
                                       archive_read_new
                                     , archive_read_data_skip
                                     , archive_read_data
                                     , archive_read_data_block
                                     , archive_read_next_header
                                     , archive_read_free
                                     , archive_read_extract
                                     , archive_read_open_filename
                                     , archive_read_open_filename_w
                                     , archive_read_open_memory
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
                                     , archive_read_set_passphrase
                                     -- * Direct bindings (write)
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
                                     ) where

import Data.Int (Int64)
import Codec.Archive.Types
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr (FunPtr, Ptr)

-- Miscellaneous
foreign import ccall archive_version_number :: CInt
foreign import ccall archive_version_string :: CString
foreign import ccall archive_version_details :: CString

type ArchiveReadCallback = FunPtr (Ptr Archive -> CString -> Ptr CString -> IO CSize)
type ArchiveSkipCallback = FunPtr (Ptr Archive -> CString -> Int64 -> IO Int64)
type ArchiveSeekCallback = FunPtr (Ptr Archive -> CString -> Int64 -> CInt -> IO Int64)
type ArchiveWriteCallback = FunPtr (Ptr Archive -> CString -> CString -> CSize -> IO CSize)
type ArchiveOpenCallback = FunPtr (Ptr Archive -> CString -> IO ArchiveError)
type ArchiveCloseCallback = FunPtr (Ptr Archive -> CString -> IO ArchiveError)
type ArchiveSwitchCallback = FunPtr (Ptr Archive -> CString -> CString -> IO ArchiveError)
type ArchivePassphraseCallback = FunPtr (Ptr Archive -> CString -> IO CString)

-- Archive read
foreign import ccall unsafe archive_read_new :: IO (Ptr Archive)
foreign import ccall unsafe archive_read_data :: Ptr Archive -> CString -> CSize -> IO ArchiveError
foreign import ccall unsafe archive_read_data_block :: Ptr Archive -> Ptr CString -> Ptr CSize -> Ptr Int64 -> IO ArchiveError
foreign import ccall unsafe archive_read_data_skip :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_next_header :: Ptr Archive -> Ptr (Ptr ArchiveEntry) -> IO ArchiveError
foreign import ccall unsafe archive_read_free :: Ptr Archive -> IO ArchiveError
foreign import ccall unsafe archive_read_extract :: Ptr Archive -> Ptr ArchiveEntry -> ExtractFlags -> IO ArchiveError
foreign import ccall unsafe archive_read_open_filename :: Ptr Archive -> CString -> CSize -> IO ArchiveError
foreign import ccall unsafe archive_read_open_filename_w :: Ptr Archive -> CWString -> CSize -> IO ArchiveError
foreign import ccall unsafe archive_read_open_memory :: Ptr Archive -> Ptr CChar -> CSize -> IO ArchiveError
foreign import ccall unsafe archive_read_add_passphrase :: Ptr Archive -> CString -> IO ArchiveError
foreign import ccall unsafe archive_read_set_passphrase :: Ptr Archive -> CString -> ArchivePassphraseCallback -> IO ArchiveError

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

-- Archive write
foreign import ccall unsafe archive_write_data :: Ptr Archive -> CString -> CSize -> IO CSize
foreign import ccall unsafe archive_write_new :: IO (Ptr Archive)
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
