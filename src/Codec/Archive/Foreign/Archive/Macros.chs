module Codec.Archive.Foreign.Archive.Macros ( archiveVersionNumber
                                            , archiveVersionOnlyString
                                            , archiveVersionString
                                            , archiveOk
                                            , archiveEOF
                                            , archiveWarn
                                            , archiveRetry
                                            , archiveFailed
                                            , archiveFatal
                                            , archiveReadFormatCapsNone
                                            , archiveReadFormatCapsEncryptData
                                            , archiveReadFormatCapsEncryptMetadata
                                            , archiveMatchMTime
                                            , archiveMatchCTime
                                            , archiveMatchNewer
                                            , archiveMatchOlder
                                            , archiveMatchEqual
                                            , archiveExtractOwner
                                            , archiveExtractPerm
                                            , archiveExtractNoOverwrite
                                            , archiveExtractUnlink
                                            , archiveExtractACL
                                            , archiveExtractFFlags
                                            , archiveExtractXattr
                                            , archiveExtractSecureSymlinks
                                            , archiveExtractSecureNoDotDot
                                            , archiveExtractTime
                                            , archiveExtractNoAutodir
                                            , archiveExtractSparse
                                            , archiveExtractMacMetadata
                                            , archiveExtractNoHfsCompression
                                            , archiveExtractHfsCompressionForced
                                            , archiveExtractSecureNoAbsolutePaths
                                            , archiveExtractClearNoChangeFFlags
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
                                            , archiveReadDiskRestoreATime
                                            , archiveReadDiskHonorNoDump
                                            , archiveReadDiskMacCopyFile
                                            , archiveReadDiskNoTraverseMounts
                                            , archiveReadDiskNoXattr
                                            -- * Conversion functions
                                            , resultToErr
                                            , errorRes
                                            , encryptionResult
                                            ) where

import Codec.Archive.Types
import Data.Bits (Bits (..))
import Foreign.C.Types

#include <archive.h>

archiveVersionNumber :: Int
archiveVersionNumber = {# const ARCHIVE_VERSION_NUMBER #}

archiveVersionOnlyString :: String
archiveVersionOnlyString = {# const ARCHIVE_VERSION_ONLY_STRING #}

archiveVersionString :: String
archiveVersionString = {# const ARCHIVE_VERSION_STRING #}

resultToErr :: ArchiveResult -> ArchiveError
resultToErr ArchiveOk     = archiveOk
resultToErr ArchiveEOF    = archiveEOF
resultToErr ArchiveRetry  = archiveRetry
resultToErr ArchiveWarn   = archiveWarn
resultToErr ArchiveFailed = archiveFailed
resultToErr ArchiveFatal  = archiveFatal

errorRes :: ArchiveError -> ArchiveResult
errorRes x | x == archiveOk     = ArchiveOk
           | x == archiveEOF    = ArchiveEOF
           | x == archiveRetry  = ArchiveRetry
           | x == archiveWarn   = ArchiveWarn
           | x == archiveFailed = ArchiveFailed
           | x == archiveFatal  = ArchiveFatal
           | otherwise = error "conversion failed"

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
