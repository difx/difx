AC_INIT([mark5access], [1.5.2], [Walter Brisken <wbrisken@nrao.edu>])

AM_CONFIG_HEADER(config.h)

AC_C_BIGENDIAN

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

AM_INIT_AUTOMAKE([foreign])

AC_CONFIG_MACRO_DIR([m4])

AC_SYS_LARGEFILE
AC_PROG_CC
AM_PROG_CC_C_O
AM_PROG_LIBTOOL
AM_SANITY_CHECK

AC_CHECK_LIB(m, erf,,[AC_MSG_ERROR("need libm")])
PKG_CHECK_MODULES(FFTW3, fftw3, [hasfftw=true], [hasfftw=false])

AC_SUBST(FFTW3_CFLAGS)
AC_SUBST(FFTW3_LIBS)
AM_CONDITIONAL(WITH_FFTW, $hasfftw)

version=AC_PACKAGE_VERSION
AC_SUBST(version)

AC_OUTPUT([ \
	Makefile \
	mark5access.pc \
	mark5access.spec \
	mark5access/Makefile \
	bbsum/Makefile \
	examples/Makefile \
	doc/Makefile \
])

