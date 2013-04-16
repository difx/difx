AC_INIT([guiServer], [0.99], [John Spitzak])

AM_INIT_AUTOMAKE([foreign])

AC_PROG_CXX
AM_SANITY_CHECK

AC_LANG(C++)

AC_CHECK_LIB(pthread, pthread_create,,[AC_MSG_ERROR("need libpthread")])
FXCORR_REQUIRED_VERSION=2.2
PKG_CHECK_MODULES(FXCORR, fxcorr >= $FXCORR_REQUIRED_VERSION)
PKG_CHECK_MODULES(DIFXMESSAGE, [difxmessage >= 2.2.0])

LIBS="${FXCORR_LIBS} ${LIBS} `pkg-config --variable=StaticLibs difxmessage`"
CXXFLAGS="${CXXFLAGS} ${DIFXMESSAGE_CFLAGS} ${FXCORR_CFLAGS} -DDIFX_VERSION='\"${DIFX_VERSION}\"'" 

echo "CXXFLAGS= ${CXXFLAGS}"
echo "LIBS= ${LIBS}"
AC_OUTPUT([ \
	Makefile \
	src/Makefile \
	network/Makefile \
])
