-- | This module corresponds to @archive.h@
--
-- Functions in this module are stateful and hence take place in the 'IO'
-- monad.
module Codec.Archive.Foreign.Archive ( archiveReadHasEncryptedEntries
                                     , archiveReadDiskCanDescend
                                     , archiveReadDiskCurrentFilesystemIsSynthetic
                                     , archiveReadDiskCurrentFilesystemIsRemote
                                     -- * Direct bindings
                                     , archive_error_string
                                     , archive_format_name
                                     , archive_format
                                     , archive_clear_error
                                     , archive_set_error
                                     , archive_copy_error
                                     , archive_file_count
                                     , archive_version_number
                                     , archive_version_string
                                     , archive_version_details
                                     , archive_filter_count
                                     , archive_filter_bytes
                                     , archive_filter_code
                                     , archive_filter_name
                                     , archive_write_new
                                     , archive_write_data
                                     , archive_read_data
                                     , archive_read_new
                                     -- * Version macros
                                     , archiveVersionNumber
                                     , archiveVersionOnlyString
                                     , archiveVersionString
                                     -- * Capability macros
                                     , archiveReadFormatCapsNone
                                     , archiveReadFormatCapsEncryptData
                                     , archiveReadFormatCapsEncryptMetadata
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
                                     -- * Haskell-ized function equivalents
                                     , archiveReadNextHeader
                                     , archiveReadOpenFilename
                                     , archiveReadOpenMemory
                                     , archiveReadSetReadCallback
                                     , archiveReadSetCloseCallback
                                     , archiveReadSetCallbackData
                                     , archiveReadOpen1
                                     , archiveWriteOpenFilename
                                     , archiveWriteOpenMemory
                                     , archiveWriteClose
                                     , archiveWriteHeader
                                     , archiveFree
                                     , archiveWriteOpen
                                     , archiveWriteSetFormatPaxRestricted
                                     , archiveWriteSetFormatZip
                                     , archiveWriteSetFormat7Zip
                                     , archiveMatchExcluded
                                     , archiveMatchPathExcluded
                                     , archiveMatchExcludePatternFromFile
                                     , archiveMatchExcludePatternFromFileW
                                     , archiveMatchIncludePatternFromFile
                                     , archiveMatchIncludePatternFromFileW
                                     , archiveMatchTimeExcluded
                                     , archiveMatchOwnerExcluded
                                     , archiveReadDataSkip
                                     , archiveReadSupportFormatAll
                                     , archiveReadExtract
                                     , archiveMatchIncludeGName
                                     , archiveMatchIncludeGNameW
                                     , archiveMatchIncludeUName
                                     , archiveMatchIncludeUNameW
                                     , archiveMatchIncludeUid
                                     , archiveMatchIncludeGid
                                     , archiveReadSupportFilterAll
                                     , archiveReadSupportFilterBzip2
                                     , archiveReadSupportFilterCompress
                                     , archiveReadSupportFilterGzip
                                     , archiveReadSupportFilterGrzip
                                     , archiveReadSupportFilterLrzip
                                     , archiveReadSupportFilterLz4
                                     , archiveReadSupportFilterLzip
                                     , archiveReadSupportFilterLzma
                                     , archiveReadSupportFilterLzop
                                     , archiveReadSupportFilterNone
                                     , archiveReadSupportFilterProgram
                                     , archiveReadSupportFilterProgramSignature
                                     , archiveReadSupportFilterRpm
                                     , archiveReadSupportFilterUu
                                     , archiveReadSupportFilterXz
                                     , archiveReadSupportFormat7zip
                                     , archiveReadSupportFormatAr
                                     , archiveReadSupportFormatByCode
                                     , archiveReadSupportFormatCab
                                     , archiveReadSupportFormatCpio
                                     , archiveReadSupportFormatEmpty
                                     , archiveReadSupportFormatGnutar
                                     , archiveReadSupportFormatIso9660
                                     , archiveReadSupportFormatLha
                                     , archiveReadSupportFormatMtree
                                     , archiveReadSupportFormatRar
                                     , archiveReadSupportFormatRaw
                                     , archiveReadSupportFormatTar
                                     , archiveReadSupportFormatWarc
                                     , archiveReadSupportFormatXar
                                     , archiveReadSupportFormatZip
                                     , archiveReadSupportFormatZipStreamable
                                     , archiveReadSupportFormatZipSeekable
                                     , archiveReadSetFormat
                                     , archiveErrno
                                     -- * Abstract types
                                     , Archive
                                     -- * Haskell types
                                     , ArchiveEncryption (..)
                                     -- * Lower-level API types
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
import           Control.Composition                  ((.*), (.**), (.***),
                                                       (.****))
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

archiveMatchExcludePatternFromFile :: Ptr Archive -> CString -> Bool -> IO ArchiveResult
archiveMatchExcludePatternFromFile a str b = errorRes <$> archive_match_exclude_pattern_from_file a str (boolToInt b)

archiveMatchExcludePatternFromFileW :: Ptr Archive -> CWString -> Bool -> IO ArchiveResult
archiveMatchExcludePatternFromFileW a str b = errorRes <$> archive_match_exclude_pattern_from_file_w a str (boolToInt b)

archiveMatchIncludePatternFromFile :: Ptr Archive -> CString -> Bool -> IO ArchiveResult
archiveMatchIncludePatternFromFile a str b = errorRes <$> archive_match_include_pattern_from_file a str (boolToInt b)

archiveMatchIncludePatternFromFileW :: Ptr Archive -> CString -> Bool -> IO ArchiveResult
archiveMatchIncludePatternFromFileW a str b = errorRes <$> archive_match_include_pattern_from_file_w a str (boolToInt b)

archiveMatchTimeExcluded :: Ptr Archive -> Ptr ArchiveEntry -> IO Bool
archiveMatchTimeExcluded = fmap intToBool .* archive_match_time_excluded

archiveReadHasEncryptedEntries :: Ptr Archive -> IO ArchiveEncryption
archiveReadHasEncryptedEntries = fmap encryptionResult . archive_read_has_encrypted_entries

archiveMatchOwnerExcluded :: Ptr Archive -> Ptr ArchiveEntry -> IO Bool
archiveMatchOwnerExcluded = fmap intToBool .* archive_match_owner_excluded

archiveReadNextHeader :: Ptr Archive -> Ptr (Ptr ArchiveEntry) -> IO ArchiveResult
archiveReadNextHeader = fmap errorRes .* archive_read_next_header

archiveReadOpenFilename :: Ptr Archive -> CString -> CSize -> IO ArchiveResult
archiveReadOpenFilename = fmap errorRes .** archive_read_open_filename

archiveReadOpenMemory :: Ptr Archive -> Ptr a -> CSize -> IO ArchiveResult
archiveReadOpenMemory = fmap errorRes .** archive_read_open_memory

archiveReadSetReadCallback :: Ptr Archive -> FunPtr (ArchiveReadCallback a b) -> IO ArchiveResult
archiveReadSetReadCallback = fmap errorRes .* archive_read_set_read_callback

archiveReadSetCloseCallback :: Ptr Archive -> FunPtr (ArchiveCloseCallbackRaw a) -> IO ArchiveResult
archiveReadSetCloseCallback = fmap errorRes .* archive_read_set_close_callback

archiveReadSetCallbackData :: Ptr Archive -> Ptr a -> IO ArchiveResult
archiveReadSetCallbackData = fmap errorRes .* archive_read_set_callback_data

archiveReadOpen1 :: Ptr Archive -> IO ArchiveResult
archiveReadOpen1 = fmap errorRes . archive_read_open1

archiveWriteOpenFilename :: Ptr Archive -> CString -> IO ArchiveResult
archiveWriteOpenFilename = fmap errorRes .* archive_write_open_filename

archiveWriteOpenMemory :: Ptr Archive -> Ptr a -> CSize -> Ptr CSize -> IO ArchiveResult
archiveWriteOpenMemory = fmap errorRes .*** archive_write_open_memory

archiveWriteClose :: Ptr Archive -> IO ArchiveResult
archiveWriteClose = fmap errorRes . archive_write_close

archiveWriteHeader :: Ptr Archive -> Ptr ArchiveEntry -> IO ArchiveResult
archiveWriteHeader = fmap errorRes .* archive_write_header

archiveFree :: Ptr Archive -> IO ArchiveResult
archiveFree = fmap errorRes . archive_free

archiveWriteOpen :: Ptr Archive -> Ptr a -> FunPtr (ArchiveOpenCallbackRaw a) -> FunPtr (ArchiveWriteCallback a b) -> FunPtr (ArchiveCloseCallbackRaw a) -> IO ArchiveResult
archiveWriteOpen = fmap errorRes .**** archive_write_open

archiveMatchIncludeGNameW :: Ptr Archive -> CWString -> IO ArchiveResult
archiveMatchIncludeGNameW = fmap errorRes .* archive_match_include_gname_w

archiveMatchIncludeGName :: Ptr Archive -> CString -> IO ArchiveResult
archiveMatchIncludeGName = fmap errorRes .* archive_match_include_gname

archiveMatchIncludeUNameW :: Ptr Archive -> CWString -> IO ArchiveResult
archiveMatchIncludeUNameW = fmap errorRes .* archive_match_include_uname_w

archiveMatchIncludeUName :: Ptr Archive -> CString -> IO ArchiveResult
archiveMatchIncludeUName = fmap errorRes .* archive_match_include_uname

archiveMatchIncludeGid :: Ptr Archive -> Id -> IO ArchiveResult
archiveMatchIncludeGid = fmap errorRes .* archive_match_include_gid

archiveMatchIncludeUid :: Ptr Archive -> Id -> IO ArchiveResult
archiveMatchIncludeUid = fmap errorRes .* archive_match_include_uid

archiveReadSupportFilterAll :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFilterAll = fmap errorRes . archive_read_support_filter_all

archiveReadSupportFilterBzip2 :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFilterBzip2 = fmap errorRes . archive_read_support_filter_bzip2

archiveReadSupportFilterCompress :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFilterCompress = fmap errorRes . archive_read_support_filter_compress

archiveReadSupportFilterGzip :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFilterGzip = fmap errorRes . archive_read_support_filter_gzip

archiveReadSupportFilterGrzip :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFilterGrzip = fmap errorRes . archive_read_support_filter_grzip

archiveReadSupportFilterLrzip :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFilterLrzip = fmap errorRes . archive_read_support_filter_lrzip

archiveReadSupportFilterLz4 :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFilterLz4 = fmap errorRes . archive_read_support_filter_lz4

archiveReadSupportFilterLzip :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFilterLzip = fmap errorRes . archive_read_support_filter_lzip

archiveReadSupportFilterLzma :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFilterLzma = fmap errorRes . archive_read_support_filter_lzma

archiveReadSupportFilterLzop :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFilterLzop = fmap errorRes . archive_read_support_filter_lzop

archiveReadSupportFilterNone :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFilterNone = fmap errorRes . archive_read_support_filter_none

archiveReadSupportFilterProgram :: Ptr Archive -> CString -> IO ArchiveResult
archiveReadSupportFilterProgram = fmap errorRes .* archive_read_support_filter_program

archiveReadSupportFilterProgramSignature :: Ptr Archive -> CString -> CString -> CSize -> IO ArchiveResult
archiveReadSupportFilterProgramSignature = fmap errorRes .*** archive_read_support_filter_program_signature

archiveReadSupportFilterRpm :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFilterRpm = fmap errorRes . archive_read_support_filter_rpm

archiveReadSupportFilterUu :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFilterUu = fmap errorRes . archive_read_support_filter_uu

archiveReadSupportFilterXz :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFilterXz = fmap errorRes . archive_read_support_filter_xz

archiveReadSupportFormat7zip :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFormat7zip = fmap errorRes . archive_read_support_format_7zip

archiveReadSupportFormatAll :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFormatAll = fmap errorRes . archive_read_support_format_all

archiveReadSupportFormatAr :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFormatAr = fmap errorRes . archive_read_support_format_ar

archiveReadSupportFormatByCode :: Ptr Archive -> CInt -> IO ArchiveResult
archiveReadSupportFormatByCode = fmap errorRes .* archive_read_support_format_by_code

archiveReadSupportFormatCab :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFormatCab = fmap errorRes . archive_read_support_format_cab

archiveReadSupportFormatCpio :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFormatCpio = fmap errorRes . archive_read_support_format_cpio

archiveReadSupportFormatEmpty :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFormatEmpty = fmap errorRes . archive_read_support_format_empty

archiveReadSupportFormatGnutar :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFormatGnutar = fmap errorRes . archive_read_support_format_gnutar

archiveReadSupportFormatIso9660 :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFormatIso9660 = fmap errorRes . archive_read_support_format_iso9660

archiveReadSupportFormatLha :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFormatLha = fmap errorRes . archive_read_support_format_lha

archiveReadSupportFormatMtree :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFormatMtree = fmap errorRes . archive_read_support_format_mtree

archiveReadSupportFormatRar :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFormatRar = fmap errorRes . archive_read_support_format_rar

archiveReadSupportFormatRaw :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFormatRaw = fmap errorRes . archive_read_support_format_raw

archiveReadSupportFormatTar :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFormatTar = fmap errorRes . archive_read_support_format_tar

archiveReadSupportFormatWarc :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFormatWarc = fmap errorRes . archive_read_support_format_warc

archiveReadSupportFormatXar :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFormatXar = fmap errorRes . archive_read_support_format_xar

archiveReadSupportFormatZip :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFormatZip = fmap errorRes . archive_read_support_format_zip

archiveReadSupportFormatZipStreamable :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFormatZipStreamable = fmap errorRes . archive_read_support_format_zip_streamable

archiveReadSupportFormatZipSeekable :: Ptr Archive -> IO ArchiveResult
archiveReadSupportFormatZipSeekable = fmap errorRes . archive_read_support_format_zip_seekable

archiveReadSetFormat :: Ptr Archive -> ArchiveFormat -> IO ArchiveResult
archiveReadSetFormat = fmap errorRes .* archive_read_set_format

archiveWriteSetFormatZip :: Ptr Archive -> IO ArchiveResult
archiveWriteSetFormatZip = fmap errorRes . archive_write_set_format_zip

archiveWriteSetFormat7Zip :: Ptr Archive -> IO ArchiveResult
archiveWriteSetFormat7Zip = fmap errorRes . archive_write_set_format_7zip

archiveWriteSetFormatPaxRestricted :: Ptr Archive -> IO ArchiveResult
archiveWriteSetFormatPaxRestricted = fmap errorRes . archive_write_set_format_pax_restricted

archiveReadDataSkip :: Ptr Archive -> IO ArchiveResult
archiveReadDataSkip = fmap errorRes . archive_read_data_skip

archiveReadExtract :: Ptr Archive -> Ptr ArchiveEntry -> Flags -> IO ArchiveResult
archiveReadExtract = fmap errorRes .** archive_read_extract

archiveErrno :: Ptr Archive -> IO ArchiveResult
archiveErrno = fmap errorRes . archive_errno
