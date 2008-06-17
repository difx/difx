AC_INIT([mk5daemon], [0.4], [Walter Brisken <wbrisken@nrao.edu>])
AM_CONFIG_HEADER([config.h])

AM_INIT_AUTOMAKE

AC_PROG_CXX
AM_SANITY_CHECK

AC_CHECK_LIB(m, erf,,[AC_MSG_ERROR("need libm")])
AC_CHECK_LIB(pthread, pthread_create,,[AC_MSG_ERROR("need libpthread")])
AC_CHECK_LIB(expat, XML_Parse,,[AC_MSG_ERROR("need expat")])

PKG_CHECK_MODULES(DIFXMESSAGE, difxmessage)
PKG_CHECK_MODULES(STREAMSTOR, streamstor)
LIBS="${LIBS} ${STREAMSTOR_LIBS} /home/swc/difx/lib/libdifxmessage.a"
CFLAGS="${CFLAGS} ${DIFXMESSAGE_CFLAGS} ${STREAMSTOR_CFLAGS}"

AC_OUTPUT([ \
	Makefile \
	src/Makefile \
	utils/Makefile \
])
