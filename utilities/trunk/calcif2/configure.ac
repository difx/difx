AC_INIT([calcif2], [2.5.0], [Walter Brisken <wbrisken@nrao.edu>])
AM_CONFIG_HEADER([config.h])

AM_INIT_AUTOMAKE

AC_PROG_CC
AM_SANITY_CHECK

AC_CHECK_LIB(m, erf,,[AC_MSG_ERROR("need libm")])

PKG_CHECK_MODULES(DIFXIO, difxio >= 3.4.0)
PKG_CHECK_MODULES(GSL, gsl, [hasgsl=true], [hasgsl=false])
if test "$hasgsl" == "true"; then
	AC_DEFINE([HAVE_GSL], 1, [1=has GSL (Gnu Scientific Library)])
fi
AM_CONDITIONAL(WITH_GSL, $hasgsl)

LIBS="${LIBS} ${DIFXIO_LIBS} ${GSL_LIBS}"
CFLAGS="${CFLAGS} ${DIFXIO_CFLAGS} ${GSL_CFLAGS}"

AC_OUTPUT([ \
	Makefile \
	src/Makefile \
	utils/Makefile \
])
