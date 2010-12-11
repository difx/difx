#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.50)
AC_INIT([VDIF_SERVER], [0.1], [difxusers at googlegroups.com])
PACKAGE_VERSION=AC_PACKAGE_VERSION
AC_SUBST([PACKAGE_VERSION])
AC_SUBST([LIBRARY_VERSION])
AM_INIT_AUTOMAKE
AC_CONFIG_SRCDIR([src/vdif_server.h])
AM_CONFIG_HEADER([config.h])

# Checks for programs.
AC_PROG_CXX
AC_PROG_RANLIB

# Checks for libraries.
PKG_CHECK_MODULES(IPP, ipp)
PKG_CHECK_MODULES(DIFXIO, difxio >= 3.0)
PKG_CHECK_MODULES(FXCORR, fxcorr >= 2.0)
PKG_CHECK_MODULES(DIFXMESSAGE, [difxmessage >= 2.0.0 difxmessage < 7])
PKG_CHECK_MODULES(VDIFIO, vdifio >= 0.1)

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

CXXFLAGS="${CXXFLAGS} ${IPP_CFLAGS} ${DIFXMESSAGE_CFLAGS} ${DIFXIO_CFLAGS} ${FXCORR_CFLAGS}"
LIBS="${IPP_LIBS} ${DIFXMESSAGE_LIBS} ${DIFXIO_LIBS} ${FXCORR_LIBS}"

AC_CONFIG_FILES([Makefile src/Makefile vdif_server.pc])
AC_OUTPUT
