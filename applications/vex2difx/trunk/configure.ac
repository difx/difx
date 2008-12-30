AC_INIT([vex2difx], [0.0], [Walter Brisken <wbrisken@nrao.edu>])
AM_CONFIG_HEADER([config.h])

AC_C_BIGENDIAN

AM_INIT_AUTOMAKE

AC_PROG_CC
AC_PROG_CPP
AC_PROG_LIBTOOL
AC_PROG_YACC
AM_PROG_LEX
AM_SANITY_CHECK

AM_SANITY_CHECK

AC_CHECK_LIB(m, erf,,[AC_MSG_ERROR("need libm")])

PKG_CHECK_MODULES(DIFXIO, difxio >= 2.6)

LIBS="${LIBS} ${DIFXIO_LIBS}"
CFLAGS="${CFLAGS} ${DIFXIO_CFLAGS}"
CXXFLAGS="${CFLAGS} ${DIFXIO_CFLAGS}"

AC_OUTPUT([ \
	Makefile \
	vex/Makefile \
	vex/vex.pc \
	src/Makefile \
])
