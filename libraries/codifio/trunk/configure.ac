AC_INIT([CODIFIO], [0.2], [Chris.Philips@csiro.au])
#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.71])
PACKAGE_VERSION=AC_PACKAGE_VERSION
AC_SUBST([PACKAGE_VERSION])
AC_SUBST([LIBRARY_VERSION])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_SRCDIR([src/codifio.h])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])
AC_CHECK_SIZEOF([size_t])
AC_SYS_LARGEFILE
AX_OPENMP

AC_C_BIGENDIAN

# Checks for programs.
AC_PATH_PROG([PYTHON],[python])
AC_PROG_CC
LT_INIT

# Checks for libraries.
AC_CHECK_LIB(m, sqrt)
#AC_CHECK_LIB(pthread, pthread_create,,[AC_MSG_ERROR("need libpthread")])

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

# Checks for conditional builds


# Prepare output files

version=AC_PACKAGE_VERSION
AC_SUBST(version)

AC_CONFIG_FILES([ \
	Makefile \
	src/Makefile \
	utils/Makefile \
	codifio.pc \
])
AC_OUTPUT
