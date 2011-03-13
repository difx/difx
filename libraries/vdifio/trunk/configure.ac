AC_INIT([VDIFIO], [0.2], [difxusers at googlegroups.com])
#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.50)
PACKAGE_VERSION=AC_PACKAGE_VERSION
AC_SUBST([PACKAGE_VERSION])
AC_SUBST([LIBRARY_VERSION])
AM_INIT_AUTOMAKE
AC_CONFIG_SRCDIR([src/vdifio.h])
AM_CONFIG_HEADER([config.h])

# Checks for programs.
AC_PROG_CC
AC_PROG_RANLIB

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

# Preparte output files

version=AC_PACKAGE_VERSION
AC_SUBST(version)

AC_CONFIG_FILES([
	Makefile
	vdifio.spec
	src/Makefile
	utils/Makefile
	vdifio.pc
])
AC_OUTPUT
