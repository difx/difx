AC_INIT([difxmessage], [2.4.0], [Walter Brisken <wbrisken@nrao.edu>])

AM_CONFIG_HEADER([config.h])

#shared library versioning
LIBRARY_VERSION=0:0:0
#               | | |
#        +------+ | +---+
#        |        |     |
#     current:revision:age
#        |        |     |
#        |        |     +- increment if interfaces have been added
#        |        |        set to zero if interfaces have been     removed
#                          or changed
#        |        +- increment if source code has changed
#        |           set to zero if current is incremented
#        +- increment if interfaces have been added, removed or    changed

PACKAGE_VERSION=AC_PACKAGE_VERSION

AC_SUBST([PACKAGE_VERSION])
AC_SUBST([LIBRARY_VERSION])

version=AC_PACKAGE_VERSION
AC_SUBST(version)


AM_INIT_AUTOMAKE([foreign])

AC_PROG_CC
AC_PROG_LIBTOOL
AM_SANITY_CHECK

dnl **********************************************************
dnl **** Look for expat libraries                         ****
dnl **** cause xml parsing to be built if found.          ****
dnl **********************************************************
expat_enabled=true
AC_CHECK_HEADER([expat.h],
		[AC_DEFINE([HAVE_EXPAT_H], 1, [1=has expat])],
		[AC_MSG_ERROR([Expat not found])])
if test ${expat_enabled} = true; then
	EXPAT_LIBS="-lexpat"
else
	EXPAT_LIBS=""
fi
AM_CONDITIONAL(WITH_EXPAT, $expat_enabled)
dnl **********************************************************

LIBS=${EXPAT_LIBS}

AC_OUTPUT([
	Makefile \
	difxmessage.pc \
	difxmessage.spec \
	mark5ipc.pc \
	difxmessage/Makefile \
	mark5ipc/Makefile \
	utils/Makefile
])
