AC_INIT([mk5daemon], [0.8], [Walter Brisken <wbrisken@nrao.edu>])
AM_CONFIG_HEADER([config.h])

AC_C_BIGENDIAN

AM_INIT_AUTOMAKE

AC_PROG_CXX
AM_SANITY_CHECK

AC_CHECK_LIB(m, erf,,[AC_MSG_ERROR("need libm")])
AC_CHECK_LIB(pthread, pthread_create,,[AC_MSG_ERROR("need libpthread")])
AC_CHECK_LIB(expat, XML_Parse,,[AC_MSG_ERROR("need expat")])

PKG_CHECK_MODULES(DIFXMESSAGE, difxmessage >= 0.5)
PKG_CHECK_MODULES(STREAMSTOR, streamstor)
PKG_CHECK_MODULES(M5ACCESS, mark5access)
LIBS="${LIBS} ${STREAMSTOR_LIBS} ${prefix}/lib/libdifxmessage.a"
CFLAGS="-D_LARGE_FILE_SOURCE -D_GNU_SOURCE -D_FILE_OFFSET_BITS=64 ${CFLAGS} ${M5ACCESS_CFLAGS} ${DIFXMESSAGE_CFLAGS} ${STREAMSTOR_CFLAGS}"
CXXFLAGS="${CFLAGS}"

echo "CFLAGS=" $CFLAGS

AC_OUTPUT([ \
	Makefile \
	src/Makefile \
	utils/Makefile \
	mk5dir/Makefile
])
