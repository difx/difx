AC_INIT([calcif2], [2.5.3], [Walter Brisken <wbrisken@nrao.edu>])
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

# rpc functionality was dropped from some flavors of linux;
# tirpc provides the same API
PKG_CHECK_MODULES(TIRPC, libtirpc,
    [AC_MSG_NOTICE(libtirpc will be used in place of the traditional rpc)],
    [AC_MSG_NOTICE(libtirpc was not found and this is not a problem unless);
     AC_MSG_NOTICE(you are using a system that has no traditional rpc.)])
CPPFLAGS="${CPPFLAGS} ${TIRPC_CFLAGS}"
LIBS="${LIBS} ${TIRPC_LIBS}"

AC_OUTPUT([ \
	Makefile \
	src/Makefile \
])
