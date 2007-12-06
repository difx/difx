AC_INIT([job2difx], [1.2], [Walter Brisken <wbrisken@nrao.edu>])
AM_CONFIG_HEADER(config.h)

AM_INIT_AUTOMAKE

AC_PROG_CC
AM_SANITY_CHECK

PKG_CHECK_MODULES(DIFXIO, difxio)
LIBS=${DIFXIO_LIBS}
CFLAGS=${DIFXIO_CFLAGS}

AC_OUTPUT([ \
	Makefile \
	calcif/Makefile \
])
