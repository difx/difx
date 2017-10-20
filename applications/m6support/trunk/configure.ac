#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.59])
AC_INIT([m6support], [0.18], [gbc@haystack.mit.edu])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_SRCDIR([vdifuse.h])
AC_CONFIG_HEADERS([config.h])

# enables are for features:  yes/no or something(set)
# vdifuse
AC_ARG_ENABLE(fuse,
    AS_HELP_STRING([--enable-fuse], [enables vdifuse program]),
    [], [enable_fuse=yes])
if [ test x$enable_fuse = xyes ] ; then
    with_fuse=true
else
    with_fuse=false
fi
AM_CONDITIONAL(WITH_FUSE, $with_fuse)

# Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_PATH_PROG(PYTHON, python, false)
AC_PATH_PROG(GNUPLOT, gnuplot, false)

if [ test x$gnuplot = false ] ; then
    have_gnuplot=false
else
    have_gnuplot=true
fi
AM_CONDITIONAL(WITH_GNUPLOT, $have_gnuplot)

# Checks for libraries.
AC_CHECK_LIB(m, rint)
#AC_CHECK_LIB(rt, clock_nanosleep)
#AC_CHECK_LIB(pcap, pcap_open_offline, pcap=-lpcap, pcap=)
#AC_SUBST(PCAP_LIBS, $pcap)

# Checks for header files.
AC_CHECK_HEADERS([\
    arpa/inet.h fcntl.h netdb.h netinet/in.h\
    stdint.h stdlib.h string.h sys/socket.h sys/time.h unistd.h])
#AC_CHECK_HEADER([pcap/pcap.h], have_pcap=true, have_pcap=false)
#AM_CONDITIONAL(HAVE_PCAP, $have_pcap)

# Checks for typedefs, structures, and compiler characteristics.
##AC_TYPE_INT32_T
##AC_TYPE_INT64_T
##AC_TYPE_OFF_T
##AC_TYPE_SIZE_T
##AC_TYPE_SSIZE_T
AC_CHECK_MEMBERS([struct stat.st_blksize])
AC_STRUCT_ST_BLOCKS
AC_CHECK_MEMBERS([struct stat.st_rdev])
##AC_TYPE_UINT32_T
##AC_TYPE_UINT64_T

# Checks for library functions.
AC_FUNC_FSEEKO
AC_FUNC_MALLOC
AC_FUNC_STRTOD
AC_CHECK_FUNCS([\
    alarm memset realpath strrchr strstr\
    clock_gettime gethostbyname gettimeofday\
    inet_ntoa rint socket strchr])

# Checks for packages
# PKG_CHECK_MODULES(FFTW3, fftw3, [hasfftw=true], [hasfftw=false])
# AM_CONDITIONAL(HAVE_FFTW, $hasfftw)
PKG_CHECK_MODULES(FUSE, fuse, [hasfuse=true], [hasfuse=false])
AM_CONDITIONAL(HAVE_FUSE, $hasfuse && $with_fuse)
PKG_CHECK_MODULES([DIFXMESSAGE], [difxmessage >= 2.5.0 difxmessage < 7], [hasdifxmsg=true], [hasdifxmsg=false])
AM_CONDITIONAL(HAVE_DIFXMESSAGE, $hasdifxmsg)
if test "$hasdifxmsg" = true; then
  AC_DEFINE(HAVE_DIFXMESSAGE, 1, [Define if you have DiFXMessage library.])
fi

AC_CONFIG_FILES([Makefile])

##AM_COND_IF([WITH_FUSE],
##    [AC_SUBST(CONFIG_OPS,--enable-fuse)],
##    [AC_SUBST(CONFIG_OPS,'')])

AC_OUTPUT
#
# eof
#
