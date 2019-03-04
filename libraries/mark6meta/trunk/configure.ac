AC_INIT([mark6meta], [0.3], [Helge Rottmann <rottmann@mpifr-bonn.mpg.de>])

AM_CONFIG_HEADER([config.h])

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
AC_PROG_LIBTOOL
AM_SANITY_CHECK

AC_LANG(C++)

PKG_CHECK_MODULES(DIFXMESSAGE, [difxmessage >= 2.4.0])
#PKG_CHECK_MODULES(LIBUDEV, [libudev >= 1.0.0], [], [AC_MSG_ERROR(libudev not found)])
AC_CHECK_LIB(udev, udev_ref)
AC_CHECK_HEADERS(libudev.h)

LIBS="${LIBS} ${DIFXMESSAGE_LIBS} ${LIBUDEV_LIBS}"
CXXFLAGS="${CXXFLAGS} ${DIFXMESSAGE_CFLAGS} ${LIBUDEV_CFLAGS}"

version=AC_PACKAGE_VERSION
AC_SUBST(version)

AC_OUTPUT([
	Makefile \
	mark6meta.pc \
	src/Makefile \
])
