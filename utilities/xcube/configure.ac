AC_INIT([xcube], [0.1], [Walter Brisken <wbrisken@nrao.edu>], [xcube])

AM_CONFIG_HEADER([config.h])

AM_INIT_AUTOMAKE([foreign])

AC_PROG_CXX

AC_PROG_CC

AC_CHECK_LIB(pthread, pthread_create,,[AC_MSG_ERROR("need libpthread")])
#AC_CHECK_LIB(rt, timer_gettime,,[AC_MSG_ERROR("need librt")])

PKG_CHECK_MODULES(VDIFIO, vdifio >= 0.1)

version=AC_PACKAGE_VERSION

AC_OUTPUT([
	Makefile \
	src/Makefile \
])
