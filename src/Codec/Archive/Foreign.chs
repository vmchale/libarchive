module Codec.Archive.Foreign ( -- * cbits
                               unpack_in_dir
                             , unpack_from_file
                             -- * Direct bindings
                             , archive_read_new
                             , archive_read_support_format_all
                             , archive_entry_set_pathname
                             , archive_read_data_skip
                             , archive_read_next_header
                             , archive_read_free
                             , archive_read_extract
                             , archive_entry_pathname
                             , archive_entry_pathname_utf8
                             , archive_read_open_filename
                             , archive_entry_new
                             -- * Header read macros
                             , archiveOk
                             , archiveEOF
                             , archiveRetry
                             , archiveWarn
                             , archiveFailed
                             , archiveFatal
                             -- * Entry flag macros
                             , archiveExtractOwner
                             , archiveExtractPerm
                             , archiveExtractTime
                             , archiveExtractNoOverwrite
                             , archiveExtractUnlink
                             , archiveExtractACL
                             ) where

import           Codec.Archive.Types
import           Data.Word           (Word)
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr

foreign import ccall unsafe unpack_in_dir :: CString -> Ptr CChar -> Word -> IO ()
foreign import ccall unsafe unpack_from_file :: CString -> CString -> IO ()

foreign import ccall unsafe archive_read_new :: IO (Ptr Archive)
foreign import ccall unsafe archive_read_support_format_all :: Ptr Archive -> IO ()
foreign import ccall unsafe archive_entry_set_pathname :: Ptr ArchiveEntry -> CString -> IO ()
foreign import ccall unsafe archive_read_data_skip :: Ptr Archive -> IO ()
foreign import ccall unsafe archive_read_next_header :: Ptr Archive -> Ptr (Ptr ArchiveEntry) -> IO ReadResult
foreign import ccall unsafe archive_read_free :: Ptr Archive -> IO ()
foreign import ccall unsafe archive_read_extract :: Ptr Archive -> Ptr ArchiveEntry -> ExtractFlags -> IO ()
foreign import ccall unsafe archive_entry_pathname :: Ptr ArchiveEntry -> IO CString
foreign import ccall unsafe archive_read_open_filename :: Ptr Archive -> CString -> CInt -> IO ()
foreign import ccall unsafe archive_entry_new :: IO (Ptr ArchiveEntry)
foreign import ccall unsafe archive_entry_pathname_utf8 :: Ptr ArchiveEntry -> IO CString

#include <archive.h>

archiveOk :: ReadResult
archiveOk = {# const ARCHIVE_OK #}

archiveEOF :: ReadResult
archiveEOF = {# const ARCHIVE_EOF #}

archiveRetry :: ReadResult
archiveRetry = {# const ARCHIVE_RETRY #}

archiveWarn :: ReadResult
archiveWarn = {# const ARCHIVE_WARN #}

archiveFailed :: ReadResult
archiveFailed = {# const ARCHIVE_FAILED #}

archiveFatal :: ReadResult
archiveFatal = {# const ARCHIVE_FATAL #}

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

-- TODO: archive.h line 667 onwards...
