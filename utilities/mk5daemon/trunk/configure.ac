AC_INIT([mk5daemon], [0.1], [Walter Brisken <wbrisken@nrao.edu>])
AM_CONFIG_HEADER([config.h])

AM_INIT_AUTOMAKE

AC_PROG_CC
AM_SANITY_CHECK

AC_CHECK_LIB(m, erf,,[AC_MSG_ERROR("need libm")])
AC_CHECK_LIB(pthread, pthread_create,,[AC_MSG_ERROR("need libpthread")])

PKG_CHECK_MODULES(DIFXMESSAGE, difxmessage)
LIBS="${LIBS} ${DIFXMESSAGE_LIBS}"
CFLAGS="${CFLAGS} ${DIFXMESSAGE_CFLAGS}"

AC_OUTPUT([ \
	Makefile \
	src/Makefile\
])
