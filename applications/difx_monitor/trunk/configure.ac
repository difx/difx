#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

# generated with autoscan, vers 2.59, hacked by gbc, wfb

AC_PREREQ(2.59)
AC_INIT([difx_monitor],[0.0],[Chris Phillips difx-users@googlegroups.com])
AM_INIT_AUTOMAKE([foreign])

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_INSTALL

# Checks for libraries.
LIBS="$LIBS"

PKG_CHECK_MODULES(DIFXMESSAGE, [difxmessage >= 2.2.0 difxmessage < 7])
PKG_CHECK_MODULES(M5ACCESS, [mark5access >= 1.4.4])
PKG_CHECK_MODULES(FXCORR, [fxcorr >= 2.2])


PKG_CHECK_MODULES(PLPLOT, plplot, [hasplplot=true], [hasplplot=false])
if test "$hasplplot" == "false"; then
  PKG_CHECK_MODULES(PLPLOT, plplotd, [hasplplot=true], [hasplplot=false])
fi
if test "$hasplplot" == "false"; then
  AC_MSG_NOTICE([PLPLOT was not found on your system.  Some utilities wont be built])
fi
AM_CONDITIONAL(WITH_PLPLOT, $hasplplot)

CXXFLAGS="${CXXFLAGS} ${M5ACCESS_CFLAGS} ${DIFXMESSAGE_CFLAGS} ${FXCORR_CFLAGS} ${PLPLOT_CFLAGS}"
LIBS=" ${FXCORR_LIBS} ${M5ACCESS_LIBS} ${DIFXMESSAGE_LIBS} ${PLPLOT_LIBS}"


# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([arpa/inet.h limits.h netdb.h netinet/in.h stdint.h])
AC_CHECK_HEADERS([stdlib.h string.h sys/socket.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_C_INLINE
AC_HEADER_TIME

# Checks for library functions.
AC_FUNC_MALLOC
AC_TYPE_SIGNAL
AC_CHECK_FUNCS([gethostbyname inet_ntoa memset socket])

AC_OUTPUT([
	Makefile \
])
