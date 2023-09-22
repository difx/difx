AC_INIT([beamformer], [0.1.0], [difxusers at googlegroups.com])
#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.50)
PACKAGE_VERSION=AC_PACKAGE_VERSION
AC_SUBST([PACKAGE_VERSION])
AC_SUBST([LIBRARY_VERSION])
AC_CONFIG_SRCDIR([src/Beamformer.h])

AM_INIT_AUTOMAKE
AM_CONFIG_HEADER([config.h])

# Checks for programs.
AC_PROG_CC
AC_PROG_CXX
AC_PROG_RANLIB
AC_PROG_LIBTOOL

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

# Preparte output files

version=AC_PACKAGE_VERSION
AC_SUBST(version)

AC_CONFIG_FILES([
	Makefile
	beamformer.spec
	beamformer.pc
	src/Makefile
	examples/Makefile
])
AC_OUTPUT
