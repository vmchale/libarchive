m4_define([LIBARCHIVE_VERSION_S],[3.8.6])
m4_define([LIBARCHIVE_VERSION_N],[3008006])

dnl bsdtar and bsdcpio versioning tracks libarchive
m4_define([BSDTAR_VERSION_S],LIBARCHIVE_VERSION_S())
m4_define([BSDCPIO_VERSION_S],LIBARCHIVE_VERSION_S())
m4_define([BSDCAT_VERSION_S],LIBARCHIVE_VERSION_S())
m4_define([BSDUNZIP_VERSION_S],LIBARCHIVE_VERSION_S())

AC_PREREQ([2.71])
AC_INIT([Haskell libarchive-clib package],[3.7.2],[vamchale@gmail.com],[libarchive-clib])

#
# Now starts the "real" configure script.
#

# Make sure the srcdir contains "libarchive" directory
AC_CONFIG_SRCDIR([c])
# Use auxiliary subscripts from this subdirectory (cleans up root)
AC_CONFIG_AUX_DIR([build/autoconf])
# M4 scripts
AC_CONFIG_MACRO_DIR([build/autoconf])

# Libtool's "interface version" can be computed from the libarchive version.

# Libtool interface version bumps on any API change, so increments
# whenever libarchive minor version does.
ARCHIVE_MINOR=$(( (LIBARCHIVE_VERSION_N() / 1000) % 1000 ))
# Libarchive 2.7 == libtool interface 9 = 2 + 7
# Libarchive 2.8 == libtool interface 10 = 2 + 8
# Libarchive 2.9 == libtool interface 11 = 2 + 9
# Libarchive 3.0 == libtool interface 12
# Libarchive 3.1 == libtool interface 13
ARCHIVE_INTERFACE=`echo $((13 + ${ARCHIVE_MINOR}))`
# Libarchive revision is bumped on any source change === libtool revision
ARCHIVE_REVISION=$(( LIBARCHIVE_VERSION_N() % 1000 ))
# Libarchive minor is bumped on any interface addition === libtool age
ARCHIVE_LIBTOOL_VERSION=$ARCHIVE_INTERFACE:$ARCHIVE_REVISION:$ARCHIVE_MINOR

# Stick the version numbers into config.h
AC_DEFINE([__LIBARCHIVE_CONFIG_H_INCLUDED], [1],
	[Internal macro for sanity checks])
AC_DEFINE([LIBARCHIVE_VERSION_STRING],"LIBARCHIVE_VERSION_S()",
	[Version number of libarchive])
AC_DEFINE_UNQUOTED([LIBARCHIVE_VERSION_NUMBER],"LIBARCHIVE_VERSION_N()",
	[Version number of libarchive as a single integer])
AC_DEFINE([BSDCPIO_VERSION_STRING],"BSDCPIO_VERSION_S()",
	[Version number of bsdcpio])
AC_DEFINE([BSDTAR_VERSION_STRING],"BSDTAR_VERSION_S()",
	[Version number of bsdtar])
AC_DEFINE([BSDCAT_VERSION_STRING],"BSDTAR_VERSION_S()",
	[Version number of bsdcat])
AC_DEFINE([BSDUNZIP_VERSION_STRING],"BSDUNZIP_VERSION_S()",
	[Version number of bsdunzip])

# The shell variables here must be the same as the AC_SUBST() variables
# below, but the shell variable names apparently cannot be the same as
# the m4 macro names above.  Why?  Ask autoconf.
BSDCPIO_VERSION_STRING=BSDCPIO_VERSION_S()
BSDTAR_VERSION_STRING=BSDTAR_VERSION_S()
BSDCAT_VERSION_STRING=BSDCAT_VERSION_S()
BSDUNZIP_VERSION_STRING=BSDUNZIP_VERSION_S()
LIBARCHIVE_VERSION_STRING=LIBARCHIVE_VERSION_S()
LIBARCHIVE_VERSION_NUMBER=LIBARCHIVE_VERSION_N()

# Substitute the above version numbers into the various files below.
# Yes, I believe this is the fourth time we define what are essentially
# the same symbols.  Why? Ask autoconf.
AC_SUBST(ARCHIVE_LIBTOOL_VERSION)
AC_SUBST(BSDCPIO_VERSION_STRING)
AC_SUBST(BSDTAR_VERSION_STRING)
AC_SUBST(BSDCAT_VERSION_STRING)
AC_SUBST(BSDUNZIP_VERSION_STRING)
AC_SUBST(LIBARCHIVE_VERSION_STRING)
AC_SUBST(LIBARCHIVE_VERSION_NUMBER)

AC_CONFIG_HEADERS([c/config.h])

# Check for host type
AC_CANONICAL_HOST

dnl Compilation on mingw and Cygwin needs special Makefile rules
inc_windows_files=no
inc_cygwin_files=no
case "$host_os" in
  *mingw* ) inc_windows_files=yes ;;
  *cygwin* | *msys*) inc_cygwin_files=yes ;;
esac
AM_CONDITIONAL([INC_WINDOWS_FILES], [test $inc_windows_files = yes])
AM_CONDITIONAL([INC_CYGWIN_FILES], [test $inc_cygwin_files = yes])

# Checks for programs.
AC_PROG_CC
AM_PROG_CC_C_O
AC_PROG_CPP
AC_USE_SYSTEM_EXTENSIONS

LT_INIT([win32-dll])
AC_CHECK_TOOL([STRIP],[strip])
AC_PROG_MKDIR_P


# Set up defines needed before including any headers
case $host in
  *mingw* | *cygwin* | *msys*  )
  AC_PREPROC_IFELSE([AC_LANG_PROGRAM(
    [[#include <windows.h>
     #ifdef _WIN32_WINNT
     # error _WIN32_WINNT already defined
     #endif
    ]],[[;]])
  ],[
    AC_DEFINE([_WIN32_WINNT], 0x0502, [Define to '0x0502' for Windows Server 2003 APIs.])
    AC_DEFINE([NTDDI_VERSION], 0x05020000, [Define to '0x05020000' for Windows Server 2003 APIs.])
  ])
  AC_PREPROC_IFELSE([AC_LANG_PROGRAM(
    [[#include <windows.h>
     #ifdef WINVER
     # error WINVER already defined
     #endif
    ]],[[;]])
  ],[
    AC_DEFINE([WINVER], 0x0502, [Define to '0x0502' for Windows Server 2003 APIs.])
  ])
  ;;
esac

# Checks for header files.
AC_HEADER_DIRENT
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([acl/libacl.h attr/xattr.h])
AC_CHECK_HEADERS([copyfile.h ctype.h])
AC_CHECK_HEADERS([errno.h ext2fs/ext2_fs.h fcntl.h fnmatch.h grp.h])

AC_CACHE_CHECK([whether EXT2_IOC_GETFLAGS is usable],
    [ac_cv_have_decl_EXT2_IOC_GETFLAGS],
    [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([@%:@include <sys/ioctl.h>
@%:@include <ext2fs/ext2_fs.h>],
                                   [int x = EXT2_IOC_GETFLAGS])],
                  [AS_VAR_SET([ac_cv_have_decl_EXT2_IOC_GETFLAGS], [yes])],
                  [AS_VAR_SET([ac_cv_have_decl_EXT2_IOC_GETFLAGS], [no])])])

AS_VAR_IF([ac_cv_have_decl_EXT2_IOC_GETFLAGS], [yes],
    [AC_DEFINE_UNQUOTED([HAVE_WORKING_EXT2_IOC_GETFLAGS], [1],
                    [Define to 1 if you have a working EXT2_IOC_GETFLAGS])])

AC_CHECK_HEADERS([inttypes.h io.h langinfo.h limits.h])
AC_CHECK_HEADERS([linux/fiemap.h linux/fs.h linux/magic.h linux/types.h])

AC_CACHE_CHECK([whether FS_IOC_GETFLAGS is usable],
    [ac_cv_have_decl_FS_IOC_GETFLAGS],
    [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([@%:@include <sys/ioctl.h>
@%:@include <linux/fs.h>],
                                   [int x = FS_IOC_GETFLAGS])],
                  [AS_VAR_SET([ac_cv_have_decl_FS_IOC_GETFLAGS], [yes])],
                  [AS_VAR_SET([ac_cv_have_decl_FS_IOC_GETFLAGS], [no])])])

AS_VAR_IF([ac_cv_have_decl_FS_IOC_GETFLAGS], [yes],
    [AC_DEFINE_UNQUOTED([HAVE_WORKING_FS_IOC_GETFLAGS], [1],
                    [Define to 1 if you have a working FS_IOC_GETFLAGS])])

AC_CHECK_HEADERS([locale.h membership.h paths.h poll.h pthread.h pwd.h])
AC_CHECK_HEADERS([readpassphrase.h signal.h spawn.h])
AC_CHECK_HEADERS([stdarg.h stdint.h stdlib.h string.h])
AC_CHECK_HEADERS([sys/acl.h sys/cdefs.h sys/ea.h sys/extattr.h])
AC_CHECK_HEADERS([sys/ioctl.h sys/mkdev.h sys/mount.h])
AC_CHECK_HEADERS([sys/param.h sys/poll.h sys/richacl.h])
AC_CHECK_HEADERS([sys/select.h sys/statfs.h sys/statvfs.h sys/sysmacros.h])
AC_CHECK_HEADERS([sys/time.h sys/utime.h sys/utsname.h sys/vfs.h sys/xattr.h])
AC_CHECK_HEADERS([time.h unistd.h utime.h wchar.h wctype.h])
AC_CHECK_TYPE([suseconds_t])
AC_CHECK_HEADERS([windows.h])
# check windows.h first; the other headers require it.
AC_CHECK_HEADERS([wincrypt.h winioctl.h],[],[],
[[#ifdef HAVE_WINDOWS_H
# include <windows.h>
#endif
]])

# Checks for libraries.
AC_ARG_WITH([zlib],
  AS_HELP_STRING([--without-zlib], [Don't build support for gzip through zlib]))

if test "x$with_zlib" = "xyes" || test "$CABAL_FLAG_zlib" = 1; then
  old_LIBS="$LIBS"
  LIBS="$LIBS -lz"
  AC_LINK_IFELSE([AC_LANG_SOURCE([[
      #include <zlib.h>
      #if !defined(ZLIB_VERNUM)
      // zlib 1.2.0 should work too, but it's difficult to test for.
      // zlib 1.2.1 onwards have ZLIB_VERNUM, which is easy to check.
      #error zlib >= 1.2.1 is required.
      #endif
      // Check that there's an inflate function.
      int main(int argc, char **argv) { inflate(NULL, 0); return 0; }
    ]])],
    [AC_DEFINE([HAVE_ZLIB_H], [1], [Define to 1 if you have zlib >= 1.2.1])
      AC_DEFINE([HAVE_LIBZ], [1], [Define to 1 if you have zlib >= 1.2.1])
      AC_MSG_RESULT([found a suitable version of zlib (>= 1.2.1)])
    ],
    [AC_MSG_ERROR([could not find a suitable version of zlib (>= 1.2.1)])])
fi

AC_ARG_WITH([bz2lib],
  AS_HELP_STRING([--without-bz2lib], [Don't build support for bzip2 through bz2lib]))

if test "x$with_bz2lib" = "xyes" || test "$CABAL_FLAG_bz2lib" = 1; then
  AC_CHECK_HEADERS([bzlib.h])
  case "$host_os" in
    *mingw* | *cygwin* | *msys*)
      dnl AC_CHECK_LIB cannot be used on the Windows port of libbz2, therefore
	  dnl use AC_LINK_IFELSE.
	  AC_MSG_CHECKING([for BZ2_bzDecompressInit in -lbz2])
      old_LIBS="$LIBS"
      LIBS="-lbz2 $LIBS"
      AC_LINK_IFELSE(
        [AC_LANG_SOURCE(#include <bzlib.h>
          int main() { return BZ2_bzDecompressInit(NULL, 0, 0); })],
        [ac_cv_lib_bz2_BZ2_bzDecompressInit=yes],
        [ac_cv_lib_bz2_BZ2_bzDecompressInit=no])
      LIBS="$old_LIBS"
	  AC_MSG_RESULT($ac_cv_lib_bz2_BZ2_bzDecompressInit)
      if test "x$ac_cv_lib_bz2_BZ2_bzDecompressInit" = xyes; then
        AC_DEFINE([HAVE_LIBBZ2], [1], [Define to 1 if you have the `bz2' library (-lbz2).])
        LIBS="-lbz2 $LIBS"
      else
		AC_MSG_ERROR([Could not find a suitable version of bz2lib])
      fi
    ;;
    *)
      AC_CHECK_LIB(bz2,BZ2_bzDecompressInit)
    ;;
  esac
fi

PKG_PROG_PKG_CONFIG

AM_CONDITIONAL([INC_BLAKE2], [test "x$ac_cv_lib_b2_blake2sp_init" != "xyes"])

AC_ARG_WITH([iconv],
  AS_HELP_STRING([--without-iconv], [Don't try to link against iconv]))

if test "x$with_iconv" = "xyes" || test "$CABAL_FLAG_iconv" = 1; then
  AM_ICONV
  AC_CHECK_HEADERS([iconv.h],[],[],[#include <stdlib.h>])
  if test "x$am_cv_func_iconv" = "xyes"; then
    AC_CHECK_HEADERS([localcharset.h])
    LIBS="${LIBS} ${LIBICONV}"
    if test -n "$LIBICONV"; then
      AC_DEFINE([HAVE_LIBICONV], [1], [Define to 1 if you have the `iconv' library (-liconv).])

      # Most platforms do not provide iconv.pc, but MSYS2 MinGW does.
      # We add it to our Requires.private only if it exists.
      PKG_CHECK_MODULES(ICONV_PC, [iconv], [
        LIBSREQUIRED="$LIBSREQUIRED${LIBSREQUIRED:+ }iconv"
      ], [true])
    fi
    AC_CHECK_FUNCS([locale_charset])
    if test "x$ac_cv_func_locale_charset" != "xyes"; then
      # If locale_charset() is not in libiconv, we have to find libcharset.
      AC_CHECK_LIB(charset,locale_charset)
    fi
  fi
fi

AC_ARG_WITH([lz4],
  AS_HELP_STRING([--without-lz4], [Don't build support for lz4 through liblz4]))

if test "x$with_lz4" = "xyes" || test "$CABAL_FLAG_lz4" = 1; then
  AC_CHECK_HEADERS([lz4.h lz4hc.h])
  AC_CHECK_LIB(lz4,LZ4_decompress_safe)
fi

AC_ARG_WITH([zstd],
  AS_HELP_STRING([--without-zstd], [Don't build support for zstd through libzstd]))

if test "x$with_zstd" = "xyes" || test "$CABAL_FLAG_zstd" = 1; then
  AC_CHECK_HEADERS([zstd.h])
  AC_CHECK_LIB(zstd,ZSTD_decompressStream)
  AC_CHECK_LIB(zstd,ZSTD_compressStream,
    AC_DEFINE([HAVE_ZSTD_compressStream], [1], [Define to 1 if you have the `zstd' library (-lzstd) with compression support.]))
  AC_CHECK_LIB(zstd,ZSTD_minCLevel,
    AC_DEFINE([HAVE_ZSTD_minCLevel], [1], [Define to 1 if you have a `zstd' library version with ZSTD_minCLevel().]),
    AC_MSG_ERROR([Could not find a suitable version of zstr]))
fi

AC_ARG_WITH([lzma],
  AS_HELP_STRING([--without-lzma], [Don't build support for xz through lzma]))

if test "x$with_lzma" = "xyes" || test "$CABAL_FLAG_lzma" = 1; then
  PKG_PROG_PKG_CONFIG
  PKG_CHECK_MODULES(LZMA_PC, [liblzma], [
    CFLAGS="${CFLAGS} ${LZMA_PC_CFLAGS}"
    LIBS="${LIBS} ${LZMA_PC_LIBS}"
    AC_DEFINE(HAVE_LIBLZMA, [1], [Define to 1 if you have the `lzma' library.])
  ], [
    AC_CHECK_LIB(lzma,lzma_stream_decoder, [], AC_MSG_ERROR([Could not find a suitable version of lzma]))
  ])
  AC_CHECK_HEADERS([lzma.h])

  # Some pre-release (but widely distributed) versions of liblzma
  # included a disabled version of lzma_stream_encoder_mt that
  # fools a naive AC_CHECK_LIB or AC_CHECK_FUNC, so we need
  # to do something more complex here:
  AC_CACHE_CHECK(
    [whether we have multithread support in lzma],
    ac_cv_lzma_has_mt,
    [AC_LINK_IFELSE([
      AC_LANG_PROGRAM([[#include <lzma.h>]
                       [#if LZMA_VERSION < 50020000]
                       [#error unsupported]
                       [#endif]],
                      [[int ignored __attribute__((unused)); ignored = lzma_stream_encoder_mt(0, 0);]])],
      [ac_cv_lzma_has_mt=yes], [ac_cv_lzma_has_mt=no])])
  if test "x$ac_cv_lzma_has_mt" != xno; then
	  AC_DEFINE([HAVE_LZMA_STREAM_ENCODER_MT], [1], [Define to 1 if you have the `lzma_stream_encoder_mt' function.])
  fi
fi


# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
# la_TYPE_UID_T defaults to "int", which is incorrect for MinGW
# and MSVC. Use a customized version.
la_TYPE_UID_T
AC_TYPE_MODE_T
# AC_TYPE_OFF_T defaults to "long", which limits us to 4GB files on
# most systems... default to "long long" instead.
AC_CHECK_TYPE(off_t, [long long])
AC_TYPE_SIZE_T
AC_CHECK_TYPE(id_t, [unsigned long])
AC_CHECK_TYPE(uintptr_t, [unsigned int])

# Check for tm_gmtoff in struct tm
AC_CHECK_MEMBERS([struct tm.tm_gmtoff, struct tm.__tm_gmtoff],,,
[
#include <time.h>
])

# Check for f_namemax in struct statfs
AC_CHECK_MEMBERS([struct statfs.f_namemax],,,
[
#include <sys/param.h>
#include <sys/mount.h>
])

# Check for f_iosize in struct statfs
AC_CHECK_MEMBERS([struct statfs.f_iosize],,,
[
#include <sys/param.h>
#include <sys/mount.h>
])

# Check for f_iosize in struct statvfs
AC_CHECK_MEMBERS([struct statvfs.f_iosize],,,
[
#include <sys/statvfs.h>
])

# Check for birthtime in struct stat
AC_CHECK_MEMBERS([struct stat.st_birthtime])

# Check for high-resolution timestamps in struct stat
AC_CHECK_MEMBERS([struct stat.st_birthtimespec.tv_nsec])
AC_CHECK_MEMBERS([struct stat.st_mtimespec.tv_nsec])
AC_CHECK_MEMBERS([struct stat.st_mtim.tv_nsec])
AC_CHECK_MEMBERS([struct stat.st_mtime_n]) # AIX
AC_CHECK_MEMBERS([struct stat.st_umtime]) # Tru64
AC_CHECK_MEMBERS([struct stat.st_mtime_usec]) # Hurd
# Check for block size support in struct stat
AC_CHECK_MEMBERS([struct stat.st_blksize])
# Check for st_flags in struct stat (BSD fflags)
AC_CHECK_MEMBERS([struct stat.st_flags])

# If you have uintmax_t, we assume printf supports %ju
# If you have unsigned long long, we assume printf supports %llu
# TODO: Check for %ju and %llu support directly.
AC_CHECK_TYPES([uintmax_t, unsigned long long])

# We use C99-style integer types
# Declare them if the local platform doesn't already do so.
AC_TYPE_INTMAX_T
AC_TYPE_UINTMAX_T
AC_TYPE_INT64_T
AC_TYPE_UINT64_T
AC_TYPE_INT32_T
AC_TYPE_UINT32_T
AC_TYPE_INT16_T
AC_TYPE_UINT16_T
AC_TYPE_UINT8_T

AC_CHECK_DECLS([SIZE_MAX, INT32_MAX, INT32_MIN])
AC_CHECK_DECLS([INT64_MAX, INT64_MIN, UINT64_MAX, UINT32_MAX])
AC_CHECK_DECLS([INTMAX_MAX, INTMAX_MIN, UINTMAX_MAX])

AC_CHECK_DECL([SSIZE_MAX],
		[AC_DEFINE(HAVE_DECL_SSIZE_MAX, 1, [Define to 1 if you have the declaration of `SSIZE_MAX', and to 0 if you don't.])],
		[],
		[#include <limits.h>])

AC_CHECK_DECL([EFTYPE],
		[AC_DEFINE(HAVE_EFTYPE, 1, [A possible errno value for invalid file format errors])],
		[],
		[#include <errno.h>])
AC_CHECK_DECL([EILSEQ],
		[AC_DEFINE(HAVE_EILSEQ, 1, [A possible errno value for invalid file format errors])],
		[],
		[#include <errno.h>])
AC_CHECK_TYPE([wchar_t],
	        [AC_DEFINE_UNQUOTED(AS_TR_CPP(HAVE_[]wchar_t), 1, [Define to 1 if the system has the type `wchar_t'.])dnl
		AC_CHECK_SIZEOF([wchar_t])],
		[])

AX_COMPILE_CHECK_SIZEOF(int)
AX_COMPILE_CHECK_SIZEOF(long)

AC_CHECK_HEADERS_ONCE([sys/time.h])

# Checks for library functions.
AC_HEADER_MAJOR
AC_FUNC_FSEEKO
AC_FUNC_MEMCMP
AC_FUNC_LSTAT
AC_FUNC_STAT
AC_FUNC_STRERROR_R
AC_FUNC_STRFTIME
AC_FUNC_VPRINTF
# check for:
#   CreateHardLinkA(LPCSTR, LPCSTR, LPSECURITY_ATTRIBUTES)
# To avoid necessity for including windows.h or special forward declaration
# workarounds, we use 'void *' for 'struct SECURITY_ATTRIBUTES *'
AC_CHECK_STDCALL_FUNC([CreateHardLinkA],[const char *, const char *, void *])
AC_CHECK_FUNCS([arc4random_buf chflags chown chroot])
AC_CHECK_FUNCS([closefrom close_range ctime_r])
AC_CHECK_FUNCS([fchdir fchflags fchmod fchown fcntl fdopendir fnmatch fork])
AC_CHECK_FUNCS([fstat fstatat fstatfs fstatvfs ftruncate])
AC_CHECK_FUNCS([futimens futimes futimesat])
AC_CHECK_FUNCS([getegid geteuid getline getpid getresgid getresuid])
AC_CHECK_FUNCS([getgrgid_r getgrnam_r getpwnam_r getpwuid_r])
AC_CHECK_FUNCS([getvfsbyname gmtime_r])
AC_CHECK_FUNCS([issetugid])
AC_CHECK_FUNCS([lchflags lchmod lchown link linkat localtime_r lstat lutimes])
AC_CHECK_FUNCS([mbrtowc memmove memset])
AC_CHECK_FUNCS([mkdir mkfifo mknod mkstemp])
AC_CHECK_FUNCS([nl_langinfo openat pipe poll posix_spawn posix_spawnp])
AC_CHECK_FUNCS([readlink readlinkat])
AC_CHECK_FUNCS([readpassphrase])
AC_CHECK_FUNCS([select setenv setlocale sigaction statfs statvfs])
AC_CHECK_FUNCS([strchr strdup strerror strncpy_s strnlen strrchr symlink])
AC_CHECK_FUNCS([sysconf])
AC_CHECK_FUNCS([tcgetattr tcsetattr])
AC_CHECK_FUNCS([timegm tzset unlinkat unsetenv utime utimensat utimes vfork])
AC_CHECK_FUNCS([wcrtomb wcscmp wcscpy wcslen wctomb wmemcmp wmemcpy wmemmove])
AC_CHECK_FUNCS([_fseeki64 _get_timezone])
AC_CHECK_DECL([cmtime_s],
		[AC_DEFINE(HAVE_CMTIME_S, 1, [cmtime_s function])],
		[],
		[#include <time.h>])
AC_CHECK_DECL([gmtime_s],
		[AC_DEFINE(HAVE_GMTIME_S, 1, [gmtime_s function])],
		[],
		[#include <time.h>])
AC_CHECK_TYPE([localtime_s],
		[AC_DEFINE(HAVE_LOCALTIME_S, 1, [localtime_s function])],
		[],
		[#include <time.h>])
AC_CHECK_DECL([_mkgmtime],
		[AC_DEFINE(HAVE__MKGMTIME, 1, [_mkgmtime function])],
		[],
		[#include <time.h>])

# detects cygwin-1.7, as opposed to older versions
AC_CHECK_FUNCS([cygwin_conv_path])

# DragonFly uses vfsconf, FreeBSD xvfsconf.
AC_CHECK_TYPES(struct vfsconf,,,
	[#if HAVE_SYS_TYPES_H
	#include <sys/types.h>
	#endif
	#include <sys/mount.h>
	])

AC_CHECK_TYPES(struct xvfsconf,,,
	[#if HAVE_SYS_TYPES_H
	#include <sys/types.h>
	#endif
	#include <sys/mount.h>
	])

AC_CHECK_TYPES(struct statfs,,,
	[#if HAVE_SYS_TYPES_H
	#include <sys/types.h>
	#endif
	#include <sys/mount.h>
	])

# dirfd can be either a function or a macro.
AC_LINK_IFELSE(
 [AC_LANG_PROGRAM([[#include <dirent.h>
                    DIR *dir;]],
                  [[return(dirfd(dir));]])],
 [AC_DEFINE(HAVE_DIRFD,1,[Define to 1 if you have a dirfd function or macro])]
)

# FreeBSD's nl_langinfo supports an option to specify whether the
# current locale uses month/day or day/month ordering.  It makes the
# output a little prettier...
AC_CHECK_DECL([D_MD_ORDER],
[AC_DEFINE(HAVE_D_MD_ORDER, 1, [Define to 1 if nl_langinfo supports D_MD_ORDER])],
[],
[#if HAVE_LANGINFO_H
#include <langinfo.h>
#endif
])

# Check for dirent.d_namlen field explicitly
# (This is a bit more straightforward than, if not quite as portable as,
# the recipe given by the autoconf maintainers.)
AC_CHECK_MEMBER(struct dirent.d_namlen,,,
[#if HAVE_DIRENT_H
#include <dirent.h>
#endif
])

# Check for Extended Attributes support
AC_ARG_ENABLE([xattr],
		AS_HELP_STRING([--disable-xattr],
		[Disable Extended Attributes support (default: check)]))

if test "x$enable_xattr" = "xyes" || test "$CABAL_FLAG_xattr" = 1; then
    AC_SEARCH_LIBS([setxattr], [attr gnu])
    AC_CHECK_DECLS([EXTATTR_NAMESPACE_USER], [], [], [#include <sys/types.h>
#include <sys/extattr.h>
])
    AC_CHECK_DECLS([XATTR_NOFOLLOW], [], [], [#include <sys/xattr.h>
])
    ATTR_PC_VER=`$PKG_CONFIG --modversion libattr`
    if test "x$ATTR_PC_VER" != "x"; then
      AC_DEFINE_UNQUOTED([LIBATTR_PKGCONFIG_VERSION], ["$ATTR_PC_VER"], [Libattr version coming from pkg-config.])
    fi
    if test "x$ac_cv_header_sys_xattr_h" = "xyes" \
	 -a "x$ac_cv_have_decl_XATTR_NOFOLLOW" = "xyes"; then
	# Darwin extended attributes support
	AC_CACHE_VAL([ac_cv_archive_xattr_darwin],
	  [AC_CHECK_FUNCS(fgetxattr \
			  flistxattr \
			  fsetxattr \
			  getxattr \
			  listxattr \
			  setxattr,
	  [ac_cv_archive_xattr_darwin=yes],
	  [ac_cv_archive_xattr_darwin=no])
	]
      )
    elif test "x$ac_cv_header_sys_extattr_h" = "xyes" \
           -a "x$ac_cv_have_decl_EXTATTR_NAMESPACE_USER" = "xyes"; then
	# FreeBSD extended attributes support
	AC_CACHE_VAL([ac_cv_archive_xattr_freebsd],
	  [AC_CHECK_FUNCS(extattr_get_fd \
			  extattr_get_file \
			  extattr_get_link \
			  extattr_list_fd \
			  extattr_list_file \
			  extattr_list_link \
			  extattr_set_fd \
			  extattr_set_link,
	  [ac_cv_archive_xattr_freebsd=yes],
	  [ac_cv_archive_xattr_freebsd=no])
	  ]
	)
    elif test "x$ac_cv_header_sys_xattr_h" = "xyes" \
	   -o "x$ac_cv_header_attr_xattr_h" = "xyes"; then
	# Linux extended attributes support
	AC_CACHE_VAL([ac_cv_archive_xattr_linux],
	  [AC_CHECK_FUNCS(fgetxattr \
			  flistxattr \
			  fsetxattr \
			  getxattr \
			  lgetxattr \
			  listxattr \
			  llistxattr \
			  lsetxattr,
	  [ac_cv_archive_xattr_linux=yes],
	  [ac_cv_archive_xattr_linux=no])
	]
      )
    elif test "x$ac_cv_header_sys_ea_h" = "xyes"; then
	# AIX extended attributes support
	AC_CACHE_VAL([ac_cv_archive_xattr_aix],
	  [AC_CHECK_FUNCS(fgetea \
			  flistea \
			  fsetea \
			  getea \
			  lgetea \
			  listea \
			  llistea \
			  lsetea,
	  [ac_cv_archive_xattr_aix=yes],
	  [ac_cv_archive_xattr_aix=no])
	  ]
	)
    fi

    AC_MSG_CHECKING([for extended attributes support])
    if test "x$ac_cv_archive_xattr_linux" = "xyes"; then
	AC_DEFINE([ARCHIVE_XATTR_LINUX], [1], [Linux xattr support])
	AC_MSG_RESULT([Linux])
    elif test "x$ac_cv_archive_xattr_darwin" = "xyes"; then
	AC_DEFINE([ARCHIVE_XATTR_DARWIN], [1], [Darwin xattr support])
	AC_MSG_RESULT([Darwin])
    elif test "x$ac_cv_archive_xattr_freebsd" = "xyes"; then
	AC_DEFINE([ARCHIVE_XATTR_FREEBSD], [1], [FreeBSD xattr support])
	AC_MSG_RESULT([FreeBSD])
    elif test "x$ac_cv_archive_xattr_aix" = "xyes"; then
	AC_DEFINE([ARCHIVE_XATTR_AIX], [1], [AIX xattr support])
	AC_MSG_RESULT([AIX])
    else
    AC_MSG_ERROR([Could not find a suitable version of xattr])
    fi
fi

# Check for ACL support
#
# The ACL support in libarchive is written against the POSIX1e draft,
# which was never officially approved and varies quite a bit across
# platforms.  Worse, some systems have completely non-POSIX acl functions,
# which makes the following checks rather more complex than I would like.
#
AC_ARG_ENABLE([acl],
		AS_HELP_STRING([--disable-acl],
		[Disable ACL support (default: check)]))

if test "x$enable_acl" = "xyes" || test "$CABAL_FLAG_acl" = 1; then
    # Libacl
    AC_CHECK_LIB([acl], [acl_get_file])
    ACL_PC_VER=`$PKG_CONFIG --modversion libacl`
    if test "x$ACL_PC_VER" != "x"; then
      AC_DEFINE_UNQUOTED([LIBACL_PKGCONFIG_VERSION], ["$ACL_PC_VER"], [Libacl version coming from pkg-config.])
    fi
    AC_CHECK_TYPES([acl_t, acl_entry_t, acl_permset_t, acl_tag_t], [], [], [
      #if HAVE_SYS_TYPES_H
      #include <sys/types.h>
      #endif
      #if HAVE_SYS_ACL_H
      #include <sys/acl.h>
      #endif
    ])

    AC_CHECK_LIB([richacl], [richacl_get_file])
    RICHACL_PC_VER=`$PKG_CONFIG --modversion librichacl`
    if test "x$RICHACL_PC_VER" != "x"; then
      AC_DEFINE_UNQUOTED([LIBRICHACL_PKGCONFIG_VERSION], ["$RICHACL_PC_VER"], [Librichacl version coming from pkg-config.])
    fi
    AC_CHECK_TYPES([[struct richace], [struct richacl]], [], [], [
      #if HAVE_SYS_RICHACL_H
      #include <sys/richacl.h>
      #endif
    ])

    # Solaris and derivates ACLs
    AC_CHECK_FUNCS(acl facl)

    if test "x$ac_cv_lib_richacl_richacl_get_file" = "xyes" \
	 -a "x$ac_cv_type_struct_richace" = "xyes" \
	 -a "x$ac_cv_type_struct_richacl" = "xyes"; then
	AC_CACHE_VAL([ac_cv_archive_acl_librichacl],
	  [AC_CHECK_FUNCS(richacl_alloc \
			  richacl_equiv_mode \
			  richacl_free \
			  richacl_get_fd \
			  richacl_get_file \
			  richacl_set_fd \
			  richacl_set_file,
	  [ac_cv_archive_acl_librichacl=yes], [ac_cv_archive_acl_librichacl=no])])
    fi

    if test "x$ac_cv_func_acl" = "xyes" \
	 -a "x$ac_cv_func_facl" = "xyes"; then
	AC_CHECK_TYPES([aclent_t], [], [], [[#include <sys/acl.h>]])
	if test "x$ac_cv_type_aclent_t" = "xyes"; then
	    AC_CACHE_VAL([ac_cv_archive_acl_sunos],
	      [AC_CHECK_DECLS([GETACL, SETACL, GETACLCNT],
	      [ac_cv_archive_acl_sunos=yes], [ac_cv_archive_acl_sunos=no],
	      [#include <sys/acl.h>])])
	    AC_CHECK_TYPES([ace_t], [], [], [[#include <sys/acl.h>]])
	    if test "x$ac_cv_type_ace_t" = "xyes"; then
		AC_CACHE_VAL([ac_cv_archive_acl_sunos_nfs4],
		  [AC_CHECK_DECLS([ACE_GETACL, ACE_SETACL, ACE_GETACLCNT],
		  [ac_cv_archive_acl_sunos_nfs4=yes],
		  [ac_cv_archive_acl_sonos_nfs4=no],
		  [#include <sys/acl.h>])])
	    fi
	fi
    elif test "x$ac_cv_type_acl_t" = "xyes" \
	 -a "x$ac_cv_type_acl_entry_t" = "xyes" \
	 -a "x$ac_cv_type_acl_permset_t" = "xyes" \
	 -a "x$ac_cv_type_acl_tag_t" = "xyes"; then
	# POSIX.1e ACL functions
	AC_CACHE_VAL([ac_cv_posix_acl_funcs],
	  [AC_CHECK_FUNCS(acl_add_perm \
			  acl_clear_perms \
			  acl_create_entry \
			  acl_delete_def_file \
			  acl_free \
			  acl_get_entry \
			  acl_get_fd \
			  acl_get_file \
			  acl_get_permset \
			  acl_get_qualifier \
			  acl_get_tag_type \
			  acl_init \
			  acl_set_fd \
			  acl_set_file \
			  acl_set_qualifier \
			  acl_set_tag_type,
	  [ac_cv_posix_acl_funcs=yes], [ac_cv_posix_acl_funcs=no])
	])

	AC_CHECK_FUNCS(acl_get_perm)

	if test "x$ac_cv_posix_acl_funcs" = "xyes" \
	     -a "x$ac_cv_header_acl_libacl_h" = "xyes" \
	     -a "x$ac_cv_lib_acl_acl_get_file" = "xyes" \
	     -a "x$ac_cv_func_acl_get_perm"; then
	    AC_CACHE_VAL([ac_cv_archive_acl_libacl],
	      [ac_cv_archive_acl_libacl=yes])
	    AC_DEFINE([ARCHIVE_ACL_LIBACL], [1],
	      [POSIX.1e ACL support via libacl])
	else
	     # FreeBSD/Darwin
	     AC_CHECK_FUNCS(acl_add_flag_np \
			    acl_clear_flags_np \
			    acl_get_brand_np \
			    acl_get_entry_type_np \
			    acl_get_flag_np \
			    acl_get_flagset_np \
			    acl_get_fd_np \
			    acl_get_link_np \
			    acl_get_perm_np \
			    acl_is_trivial_np \
			    acl_set_entry_type_np \
			    acl_set_fd_np \
			    acl_set_link_np)

	    AC_CHECK_FUNCS(mbr_uid_to_uuid \
			   mbr_uuid_to_id \
			   mbr_gid_to_uuid)

	    AC_CHECK_DECLS([ACL_TYPE_EXTENDED, ACL_TYPE_NFS4, ACL_USER,
	      ACL_SYNCHRONIZE], [], [],
	      [#include <sys/types.h>
	       #include <sys/acl.h>])
	    if test "x$ac_cv_posix_acl_funcs" = "xyes" \
	         -a "x$ac_cv_func_acl_get_fd_np" = "xyes" \
                 -a "x$ac_cv_func_acl_get_perm" != "xyes" \
	         -a "x$ac_cv_func_acl_get_perm_np" = "xyes" \
	         -a "x$ac_cv_func_acl_set_fd_np" = "xyes"; then
		if test "x$ac_cv_have_decl_ACL_USER" = "xyes"; then
		    AC_CACHE_VAL([ac_cv_archive_acl_freebsd],
		      [ac_cv_archive_acl_freebsd=yes])
		    if test "x$ac_cv_have_decl_ACL_TYPE_NFS4" = "xyes" \
		         -a "x$ac_cv_func_acl_add_flag_np" = "xyes" \
		         -a "x$ac_cv_func_acl_get_brand_np" = "xyes" \
		         -a "x$ac_cv_func_acl_get_entry_type_np" = "xyes" \
		         -a "x$ac_cv_func_acl_get_flagset_np" = "xyes" \
		         -a "x$ac_cv_func_acl_set_entry_type_np" = "xyes"; then
			AC_CACHE_VAL([ac_cv_archive_acl_freebsd_nfs4],
			  [ac_cv_archive_acl_freebsd_nfs4=yes])
		    fi
	        elif test "x$ac_cv_have_decl_ACL_TYPE_EXTENDED" = "xyes" \
		       -a "x$ac_cv_func_acl_add_flag_np" = "xyes" \
		       -a "x$ac_cv_func_acl_get_flagset_np" = "xyes" \
		       -a "x$ac_cv_func_acl_get_link_np" = "xyes" \
		       -a "x$ac_cv_func_acl_set_link_np" = "xyes" \
		       -a "x$ac_cv_func_mbr_uid_to_uuid" = "xyes" \
		       -a "x$ac_cv_func_mbr_uuid_to_id" = "xyes" \
		       -a "x$ac_cv_func_mbr_gid_to_uuid" = "xyes"; then
		    AC_CACHE_VAL([ac_cv_archive_acl_darwin],
		      [ac_cv_archive_acl_darwin=yes])
	        fi
	    fi
	fi
    fi
    AC_MSG_CHECKING([for ACL support])
    if test "x$ac_cv_archive_acl_libacl" = "xyes" \
	 -a "x$ac_cv_archive_acl_librichacl" = "xyes"; then
	AC_MSG_RESULT([libacl (POSIX.1e) + librichacl (NFSv4)])
	AC_DEFINE([ARCHIVE_ACL_LIBACL], [1],
	  [Linux POSIX.1e ACL support via libacl])
	AC_DEFINE([ARCHIVE_ACL_LIBRICHACL], [1],
	  [Linux NFSv4 ACL support via librichacl])
    elif test "x$ac_cv_archive_acl_libacl" = "xyes"; then
	AC_MSG_RESULT([libacl (POSIX.1e)])
	AC_DEFINE([ARCHIVE_ACL_LIBACL], [1],
	  [Linux POSIX.1e ACL support via libacl])
    elif test "x$ac_cv_archive_acl_librichacl" = "xyes"; then
	AC_MSG_RESULT([librichacl (NFSv4)])
	AC_DEFINE([ARCHIVE_ACL_LIBRICHACL], [1],
	  [Linux NFSv4 ACL support via librichacl])
    elif test "x$ac_cv_archive_acl_darwin" = "xyes"; then
	AC_DEFINE([ARCHIVE_ACL_DARWIN], [1], [Darwin ACL support])
	AC_MSG_RESULT([Darwin (limited NFSv4)])
    elif test "x$ac_cv_archive_acl_sunos" = "xyes"; then
	AC_DEFINE([ARCHIVE_ACL_SUNOS], [1], [Solaris ACL support])
	if test "x$ac_cv_archive_acl_sunos_nfs4" = "xyes"; then
	    AC_DEFINE([ARCHIVE_ACL_SUNOS_NFS4], [1],
	      [Solaris NFSv4 ACL support])
	    AC_MSG_RESULT([Solaris (POSIX.1e and NFSv4)])
	else
	    AC_MSG_RESULT([Solaris (POSIX.1e)])
	fi
    elif test "x$ac_cv_archive_acl_freebsd" = "xyes"; then
	AC_DEFINE([ARCHIVE_ACL_FREEBSD], [1], [FreeBSD ACL support])
	if test "x$ac_cv_archive_acl_freebsd_nfs4" = "xyes"; then
	    AC_DEFINE([ARCHIVE_ACL_FREEBSD_NFS4], [1],
	      [FreeBSD NFSv4 ACL support])
	    AC_MSG_RESULT([FreeBSD (POSIX.1e and NFSv4)])
	else
	    AC_MSG_RESULT([FreeBSD (POSIX.1e)])
	fi
    else
    AC_MSG_ERROR([Could not find a suitable version of acl])
    fi
fi


AM_CONDITIONAL([INC_LINUX_ACL],
  [test "x$ac_cv_archive_acl_libacl" = "xyes" \
     -o "x$ac_cv_archive_acl_librichacl" = "xyes"])
AM_CONDITIONAL([INC_SUNOS_ACL], [test "x$ac_cv_archive_acl_sunos" = "xyes"])
AM_CONDITIONAL([INC_DARWIN_ACL],
	  [test "x$ac_cv_archive_acl_darwin" = "xyes"])
AM_CONDITIONAL([INC_FREEBSD_ACL],
	  [test "x$ac_cv_archive_acl_freebsd" = "xyes"])

# Additional requirements
AC_SYS_LARGEFILE

AC_SUBST([EXTRA_LIBS],[` printf " %s " "$LIBS" | sed -E 's/ -l([[^ ]]*)/\1 /g' `])
AC_CONFIG_FILES([libarchive-clib.buildinfo])

AC_OUTPUT

