AC_INIT([mk5daemon], [2.3.0], [Walter Brisken <wbrisken@nrao.edu>])
AM_CONFIG_HEADER([config.h])

AC_C_BIGENDIAN

AM_INIT_AUTOMAKE([foreign])

AC_PROG_CXX
AM_SANITY_CHECK

AC_LANG(C++)

AC_CHECK_HEADER(expat.h)

AC_CHECK_LIB(m, erf,,[AC_MSG_ERROR("need libm")])
AC_CHECK_LIB(pthread, pthread_create,,[AC_MSG_ERROR("need libpthread")])
AC_CHECK_LIB(expat, XML_Parse,,[AC_MSG_ERROR("need expat")])

PKG_CHECK_MODULES(DIFXMESSAGE, [difxmessage >= 2.3.0 difxmessage < 7])
PKG_CHECK_MODULES(MARK5IPC, [mark5ipc >= 2.3.0 mark5ipc < 7])
PKG_CHECK_MODULES(M5ACCESS, [mark5access >= 1.5.0])

streamstor_enabled=true
PKG_CHECK_EXISTS([streamstor >= 8.0],
	[ PKG_CHECK_MODULES(STREAMSTOR, streamstor) ],
	[ AC_MSG_NOTICE([Did not find STREAMSTOR in $PKG_CONFIG_PATH])
	  streamstor_enabled=false])
if test ${streamstor_enabled} = true; then
	CPPFLAGS="${CPPFLAGS} ${STREAMSTOR_CFLAGS}"
	CXXFLAGS="${CXXFLAGS} ${STREAMSTOR_CFLAGS}"
	AC_CHECK_HEADERS([xlrapi.h])
else
	STREAMSTOR_CFLAGS=""
	STREAMSTOR_LIBS=""
	echo 'No suitable streamstor.pc file was found so streamstor'
	echo 'Mark5 support will be suppressed.  See the documentation'
	echo 'if you are surprised by this.'
fi
AM_CONDITIONAL(WITH_STREAMSTOR, $streamstor_enabled)

LIBS="${LIBS} ${STREAMSTOR_LIBS} `pkg-config --variable=StaticLibs difxmessage` `pkg-config --variable=StaticLibs mark5ipc` `pkg-config --variable=StaticLibs mark5access`"
CPPFLAGS="-D_LARGE_FILE_SOURCE -D_GNU_SOURCE -D_FILE_OFFSET_BITS=64 ${M5ACCESS_CFLAGS} ${DIFXMESSAGE_CFLAGS} ${MARK5IPC_CFLAGS} ${STREAMSTOR_CFLAGS} -O2 -Wall"
CXXFLAGS="${CPPFLAGS}"

version=AC_PACKAGE_VERSION
AC_SUBST(version)

echo "CPPFLAGS=${CPPFLAGS}"
echo "LIBS=${LIBS}"

AC_OUTPUT([ \
	Makefile \
	mk5daemon.spec \
	src/Makefile \
	utils/Makefile \
	mk5dir/Makefile
])
