AC_PREREQ(2.50)
AC_INIT([DIFX2PROFILE], [0.1], [difxusers at googlegroups.com])
PACKAGE_VERSION=AC_PACKAGE_VERSION
AC_SUBST([PACKAGE_VERSION])
AC_SUBST([LIBRARY_VERSION])

AM_INIT_AUTOMAKE

AC_LANG_CPLUSPLUS
AC_CONFIG_SRCDIR([src/difx2profile.h])
AM_CONFIG_HEADER([config.h])

# Checks for programs.
AC_PROG_CXX
AC_PROG_RANLIB

dnl **********************************************************
dnl **** Set path for Intel Performance Primitive library ****
dnl **********************************************************

PKG_CHECK_MODULES(IPP, ipp)

dnl **********************************************************

# Checks for libraries.
PKG_CHECK_MODULES(DIFXIO, difxio >= 2.9)
PKG_CHECK_MODULES(FXCORR, fxcorr >= 1.5)
dm_enabled=true
PKG_CHECK_MODULES(DIFXMESSAGE, difxmessage >= 0.4,
                  [AC_DEFINE([HAVE_DIFXMESSAGE], 1, [1=has DIFXMESSAGE])],
                  dm_enabled=false)
AM_CONDITIONAL(WITH_DIFXMESSAGE, $dm_enabled)

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

CXXFLAGS="${CXXFLAGS} ${IPP_CFLAGS} ${DIFXIO_CFLAGS} ${DIFXMESSAGE_CFLAGS} ${FXCORR_CFLAGS}"
LIBS="${IPP_LIBS} ${DIFXIO_LIBS} ${DIFXMESSAGE_LIBS} ${FXCORR_LIBS} -lpthread"

AC_CONFIG_FILES([Makefile src/Makefile scripts/Makefile])
AC_OUTPUT
