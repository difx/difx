AC_INIT([mk5daemon], [2.999], [Walter Brisken <wbrisken@nrao.edu>])
AC_CONFIG_HEADERS([config.h])

AC_C_BIGENDIAN

AM_INIT_AUTOMAKE([foreign])

AC_PROG_CXX
AM_SANITY_CHECK

AC_LANG(C++)

dnl Assume Mark6 until proven otherwise
hasmark6meta=true
hasmark6sg=true
hasmark6=true

AC_CHECK_HEADER(expat.h)

AC_SEARCH_LIBS([sqrt], [m])
AC_SEARCH_LIBS([pthread_create], [pthread])
AC_SEARCH_LIBS([XML_Parse], [expat])

PKG_CHECK_MODULES(DIFXMESSAGE, [difxmessage >= 2.9.0])
PKG_CHECK_MODULES(MARK5IPC, [mark5ipc >= 2.8.0])
PKG_CHECK_MODULES(M5ACCESS, [mark5access >= 1.7])
PKG_CHECK_MODULES(MARK6META, [mark6meta >= 0.4], [AC_DEFINE(HAS_MARK6META, 1)], [hasmark6meta=false])
PKG_CHECK_MODULES(MARK6SG, [mark6sg >= 2.0.4], [AC_DEFINE(HAS_MARK6SG, 1)], [hasmark6sg=false])
PKG_CHECK_MODULES(VDIFIO, vdifio >= 1.6)

AH_TEMPLATE(HAS_MARK6META, [True if mark6meta library has been found])
AH_TEMPLATE(HAS_MARK6SG, [True if mark6sg library has been found])

if test $hasmark6meta = false; then
	hasmark6=false
	echo "XXXXXX  Mark6 is false"
fi
if test $hasmark6sg = false; then
	hasmark6=false
	echo "XXXXXX  Mark6 is false"
fi

AM_CONDITIONAL(WITH_MARK6META, $hasmark6meta)
AM_CONDITIONAL(WITH_MARK6SG, $hasmark6sg)
AM_CONDITIONAL(WITH_MARK6, $hasmark6)

LIBS="${LIBS} ${VDIFIO_LIBS} ${MARK6SG_LIBS} ${M5ACCESS_LIBS} `pkg-config --variable=StaticLibs difxmessage` `pkg-config --variable=StaticLibs mark5ipc` `pkg-config --variable=StaticLibs mark5access` `pkg-config --variable=StaticLibs mark6meta` `pkg-config --variable=StaticLibs mark6sg`"
CPPFLAGS="-D_LARGE_FILE_SOURCE -D_GNU_SOURCE -D_FILE_OFFSET_BITS=64 ${M5ACCESS_CFLAGS} ${DIFXMESSAGE_CFLAGS} ${MARK6META_CFLAGS} ${MARK5IPC_CFLAGS} ${VDIFIO_CFLAGS} ${CXXFLAGS} -O2 -Wall"
CXXFLAGS="${CPPFLAGS}"

version=AC_PACKAGE_VERSION
AC_SUBST(version)

echo "CPPFLAGS=${CPPFLAGS}"
echo "LIBS=${LIBS}"

AC_CONFIG_FILES([ \
	Makefile \
	vlbi-mk5daemon.spec \
	src/Makefile \
	utils/Makefile \
	mk5dir/Makefile
])

AC_OUTPUT()
