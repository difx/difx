AC_INIT([VDIFIO], [0.4], [difxusers at googlegroups.com])
#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.50)
PACKAGE_VERSION=AC_PACKAGE_VERSION
AC_SUBST([PACKAGE_VERSION])
AC_SUBST([LIBRARY_VERSION])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_SRCDIR([src/vdifio.h])
AM_CONFIG_HEADER([config.h])
AC_SYS_LARGEFILE

AC_C_BIGENDIAN

# Checks for programs.
AC_PROG_CC
AC_PROG_LIBTOOL

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

# Preparte output files

version=AC_PACKAGE_VERSION
AC_SUBST(version)

AC_OUTPUT([ \
	Makefile \
	vdifio.spec \
	src/Makefile \
	utils/Makefile \
	vdifio.pc \
])
