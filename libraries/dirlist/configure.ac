AC_INIT([dirlist], [0.2], [Walter Brisken <wbrisken@nrao.edu>])

AC_CONFIG_HEADERS([config.h])

#shared library versioning
LIBRARY_VERSION=0:0:0
#               | | |
#        +------+ | +---+
#        |        |     |
#     current:revision:age
#        |        |     |
#        |        |     +- increment if interfaces have been added
#        |        |        set to zero if interfaces have been removed
#                          or changed
#        |        +- increment if source code has changed
#        |           set to zero if current is incremented
#        +- increment if interfaces have been added, removed or changed

PACKAGE_VERSION=AC_PACKAGE_VERSION

AC_SUBST([PACKAGE_VERSION])
AC_SUBST([LIBRARY_VERSION])

AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([foreign])

AC_PROG_CXX
LT_INIT
AM_SANITY_CHECK

AC_LANG(C++)

version=AC_PACKAGE_VERSION
AC_SUBST(version)

AC_CONFIG_FILES([
	Makefile \
	dirlist.pc \
	src/Makefile \
	tests/Makefile \
	utils/Makefile \
])
AC_OUTPUT
