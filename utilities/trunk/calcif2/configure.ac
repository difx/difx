AC_INIT([calcif2], [1.0], [Walter Brisken <wbrisken@nrao.edu>])
AM_CONFIG_HEADER(config.h)

AM_INIT_AUTOMAKE

AC_PROG_CC
AM_SANITY_CHECK

AC_CHECK_LIB(m, erf,,[AC_MSG_ERROR("need libm")])

PKG_CHECK_MODULES(DIFXIO, difxio >= 2.10)
LIBS="${LIBS} ${DIFXIO_LIBS}"
CFLAGS="${CFLAGS} ${DIFXIO_CFLAGS}"

AC_OUTPUT([ \
	Makefile \
	src/Makefile \
	utils/Makefile \
])
