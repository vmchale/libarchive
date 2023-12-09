m4_define([LIBARCHIVE_VERSION_S],[3.7.2])
m4_define([LIBARCHIVE_VERSION_N],[3007002])

dnl bsdtar and bsdcpio versioning tracks libarchive
m4_define([BSDTAR_VERSION_S],LIBARCHIVE_VERSION_S())
m4_define([BSDCPIO_VERSION_S],LIBARCHIVE_VERSION_S())
m4_define([BSDCAT_VERSION_S],LIBARCHIVE_VERSION_S())

AC_PREREQ([2.69])
AC_INIT([Haskell libarchive package],[3.0.4.2],[vamchale@gmail.com],[libarchive])

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
# Libarchive 2.9 == libtool interface 11 = 2 + 8
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

# The shell variables here must be the same as the AC_SUBST() variables
# below, but the shell variable names apparently cannot be the same as
# the m4 macro names above.  Why?  Ask autoconf.
BSDCPIO_VERSION_STRING=BSDCPIO_VERSION_S()
BSDTAR_VERSION_STRING=BSDTAR_VERSION_S()
BSDCAT_VERSION_STRING=BSDCAT_VERSION_S()
LIBARCHIVE_VERSION_STRING=LIBARCHIVE_VERSION_S()
LIBARCHIVE_VERSION_NUMBER=LIBARCHIVE_VERSION_N()

# Substitute the above version numbers into the various files below.
# Yes, I believe this is the fourth time we define what are essentially
# the same symbols.  Why? Ask autoconf.
AC_SUBST(ARCHIVE_LIBTOOL_VERSION)
AC_SUBST(BSDCPIO_VERSION_STRING)
AC_SUBST(BSDTAR_VERSION_STRING)
AC_SUBST(BSDCAT_VERSION_STRING)
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
AC_LIBTOOL_WIN32_DLL
AC_PROG_LIBTOOL
AC_CHECK_TOOL([STRIP],[strip])
AC_PROG_MKDIR_P


# Set up defines needed before including any headers
case $host in
  *mingw* | *cygwin* | *msys*  )
  AC_PREPROC_IFELSE([AC_LANG_PROGRAM(
    [[#ifdef _WIN32_WINNT
     # error _WIN32_WINNT already defined
     #endif
    ]],[[;]])
  ],[
    AC_DEFINE([_WIN32_WINNT], 0x0502, [Define to '0x0502' for Windows Server 2003 APIs.])
    AC_DEFINE([NTDDI_VERSION], 0x05020000, [Define to '0x05020000' for Windows Server 2003 APIs.])
  ])
  AC_PREPROC_IFELSE([AC_LANG_PROGRAM(
    [[#ifdef WINVER
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
AC_CHECK_HEADERS([sys/ioctl.h sys/mkdev.h sys/mount.h sys/queue.h])
AC_CHECK_HEADERS([sys/param.h sys/poll.h sys/richacl.h])
AC_CHECK_HEADERS([sys/select.h sys/statfs.h sys/statvfs.h sys/sysmacros.h])
AC_CHECK_HEADERS([sys/time.h sys/utime.h sys/utsname.h sys/vfs.h sys/xattr.h])
AC_CHECK_HEADERS([time.h unistd.h utime.h wchar.h wctype.h])
AC_CHECK_HEADERS([windows.h])
# check windows.h first; the other headers require it.
AC_CHECK_HEADERS([wincrypt.h winioctl.h],[],[],
[[#ifdef HAVE_WINDOWS_H
# include <windows.h>
#endif
]])

AM_CONDITIONAL([INC_BLAKE2], [test "x$ac_cv_lib_b2_blake2sp_init" != "xyes"])


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

AC_CHECK_HEADERS_ONCE([sys/time.h])

# Checks for library functions.
AC_PROG_GCC_TRADITIONAL
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
AC_CHECK_FUNCS([arc4random_buf chflags chown chroot ctime_r])
AC_CHECK_FUNCS([fchdir fchflags fchmod fchown fcntl fdopendir fnmatch fork])
AC_CHECK_FUNCS([fstat fstatat fstatfs fstatvfs ftruncate])
AC_CHECK_FUNCS([futimens futimes futimesat])
AC_CHECK_FUNCS([geteuid getline getpid getgrgid_r getgrnam_r])
AC_CHECK_FUNCS([getpwnam_r getpwuid_r getvfsbyname gmtime_r])
AC_CHECK_FUNCS([lchflags lchmod lchown link linkat localtime_r lstat lutimes])
AC_CHECK_FUNCS([mbrtowc memmove memset])
AC_CHECK_FUNCS([mkdir mkfifo mknod mkstemp])
AC_CHECK_FUNCS([nl_langinfo openat pipe poll posix_spawnp readlink readlinkat])
AC_CHECK_FUNCS([readpassphrase])
AC_CHECK_FUNCS([select setenv setlocale sigaction statfs statvfs])
AC_CHECK_FUNCS([strchr strdup strerror strncpy_s strnlen strrchr symlink])
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

# There are several variants of readdir_r around; we only
# accept the POSIX-compliant version.
AC_LINK_IFELSE(
 [AC_LANG_PROGRAM([[#include <dirent.h>]],
                  [[DIR *dir; struct dirent e, *r;
		    return(readdir_r(dir, &e, &r));]])],
 [AC_DEFINE(HAVE_READDIR_R,1,[Define to 1 if you have a POSIX compatible readdir_r])]
)
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

# Additional requirements
AC_SYS_LARGEFILE

AC_OUTPUT

