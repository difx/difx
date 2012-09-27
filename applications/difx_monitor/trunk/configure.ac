#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

# generated with autoscan, vers 2.59, hacked by gbc

AC_PREREQ(2.59)
AC_INIT([difx_monitor],[0.0],[Chris Phillips difx-users@googlegroups.com])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_SRCDIR([vcal.cpp])
AC_CONFIG_HEADER([config.h])

if test [ -n "$DIFX_DM_LIBS" ] ; then
    LIBS="$DIFX_DM_LIBS $LIBS"
fi
if test [ -n "$DIFX_DM_FLAGS" ] ; then
    CPPFLAGS="$DIFX_DM_FLAGS $CPPFLAGS"
fi

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_INSTALL

# Checks for libraries.
LIBS="$LIBS $FLIBS"

# the X11/PGPLOT problem is a mess.
if test [ -n "$PGPLOT_DIR" -a -z "$PGPLOTDIR" ] ; then
    PGPLOTDIR=$PGPLOT_DIR
fi
if test [ -d "$PGPLOTDIR" ] ; then
    CPPFLAGS="-I$PGPLOTDIR"
    LIBS="-L$PGPLOTDIR $LIBS"
fi
AC_CHECK_LIB([X11], [XOpenDisplay])
AC_CHECK_LIB([pgplot], [pgbeg_])
AC_CHECK_LIB([cpgplot], [cpgbeg])
AC_CHECK_LIB([png], [png_access_version_number])

AC_MSG_NOTICE([
%%%
%%% Support for X11, PGPLOT (which may require PNG)
%%% installations.  You may be able to solve problems
%%% by setting environment variables with appropriate
%%% compile (DIFX_DM_FLAGS) and link (DIFX_DM_LIBS) flags.
%%%])

# AC_CHECK_LIB([difxmessage], [main])
PKG_CHECK_MODULES(DIFXMESSAGE, [difxmessage >= 2.2.0 difxmessage < 7])
LIBS="$DIFXMESSAGE_LIBS $LIBS"
CPPFLAGS="$DIFXMESSAGE_CFLAGS $CPPFLAGS"

# AC_CHECK_LIB([mark5access], [main])
PKG_CHECK_MODULES(M5ACCESS, [mark5access >= 1.4.4])
LIBS="$M5ACCESS_LIBS $LIBS"
CPPFLAGS="$M5ACCESS_CFLAGS $CPPFLAGS"

# AC_CHECK_LIB([fxcorr], [main])
PKG_CHECK_MODULES(FXCORR, [fxcorr >= 2.2])
LIBS="$FXCORR_LIBS $LIBS"
CPPFLAGS="$FXCORR_CFLAGS $CPPFLAGS"


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

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

# eof
