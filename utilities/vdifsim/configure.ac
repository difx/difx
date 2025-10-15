AC_INIT([vdifsim],[0.2],[Walter Brisken <wbrisken@nrao.edu>])
AC_CONFIG_HEADERS([config.h])

AM_INIT_AUTOMAKE([foreign])

AC_C_BIGENDIAN

LT_INIT
AC_PROG_CC([mpicc])
AC_PROG_MAKE_SET

AM_SANITY_CHECK

AC_CONFIG_MACRO_DIR([m4])

AC_SEARCH_LIBS([sqrt], [m])
AC_CHECK_LIB(pthread, pthread_create,,[AC_MSG_ERROR("need libpthread")])

PKG_CHECK_MODULES(GLIB, glib-2.0)
PKG_CHECK_MODULES(DIFXIO, difxio >= 3.8.0)
PKG_CHECK_MODULES(VDIFIO, vdifio >= 1.6)
PKG_CHECK_MODULES(DIFXMESSAGE, [difxmessage >= 2.9.0])
PKG_CHECK_MODULES(FFTW, fftw3)
PKG_CHECK_MODULES(GSL, gsl, [hasgsl=true], [hasgsl=false])
if test "$hasgsl" == "true"; then
        AC_DEFINE([HAVE_GSL], 1, [1=has GSL (Gnu Scientific Library)])
fi
AM_CONDITIONAL(WITH_GSL, $hasgsl)

LIBS="${LIBS} ${GLIB_LIBS} ${DIFXIO_LIBS} ${DIFXMESSAGE_LIBS} ${VDIFIO_LIBS} ${GSL_LIBS} ${FFTW_LIBS}"
CFLAGS="${CFLAGS} ${GLIB_CFLAGS} ${DIFXIO_CFLAGS} ${DIFXMESSAGE_CFLAGS} ${VDIFIO_CFLAGS} ${GSL_CFLAGS} ${FFTW_CFLAGS}"
CXXFLAGS="${CFLAGS} -std=c++11 -Wno-unknown-pragmas -Wno-reorder"

AC_CONFIG_FILES([ \
	Makefile \
	src/Makefile \
])

AC_OUTPUT
