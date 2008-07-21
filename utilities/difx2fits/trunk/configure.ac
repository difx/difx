AC_INIT([difx2fits], [2.4], [Walter Brisken <wbrisken@nrao.edu>])
AM_CONFIG_HEADER([config.h])

AC_C_BIGENDIAN


AM_INIT_AUTOMAKE

AC_PROG_CC
AM_SANITY_CHECK

AC_CHECK_LIB(m, erf,,[AC_MSG_ERROR("need libm")])

PKG_CHECK_MODULES(DIFXIO, difxio >= 2.0)
LIBS="${LIBS} ${DIFXIO_LIBS}"
CFLAGS="-D_LARGE_FILE_SOURCE -D_GNU_SOURCE -D_FILE_OFFSET_BITS=64 ${CFLAGS} ${DIFXIO_CFLAGS}"

AC_OUTPUT([ \
	Makefile \
	src/Makefile\
])
