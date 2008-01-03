AC_INIT([difx2fits], [1.7], [Walter Brisken <wbrisken@nrao.edu>])
AM_CONFIG_HEADER([config.h])

AC_C_BIGENDIAN


AM_INIT_AUTOMAKE

AC_PROG_CC
AM_SANITY_CHECK

PKG_CHECK_MODULES(DIFXIO, difxio)
LIBS="${LIBS} ${DIFXIO_LIBS}"
CFLAGS="${CFLAGS} ${DIFXIO_CFLAGS}"

AC_OUTPUT([ \
	Makefile \
	src/Makefile\
])
