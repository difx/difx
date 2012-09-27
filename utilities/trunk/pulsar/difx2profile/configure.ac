AC_PREREQ(2.50)
AC_INIT([DIFX2PROFILE], [0.4], [difxusers at googlegroups.com])
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

# Checks for libraries.
PKG_CHECK_MODULES(DIFXIO, difxio >= 3.0.2)
PKG_CHECK_MODULES(FXCORR, fxcorr >= 2.0.2)
PKG_CHECK_MODULES(DIFXMESSAGE, difxmessage >= 2.0.2)

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

CXXFLAGS="${CXXFLAGS} ${DIFXIO_CFLAGS} ${DIFXMESSAGE_CFLAGS} ${FXCORR_CFLAGS}"
LIBS=" ${FXCORR_LIBS} ${DIFXIO_LIBS} ${DIFXMESSAGE_LIBS} -lpthread"

AC_CONFIG_FILES([Makefile src/Makefile scripts/Makefile])
AC_OUTPUT
