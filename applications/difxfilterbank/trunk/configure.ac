AC_INIT([difxfilterbank], [0.6])

AM_INIT_AUTOMAKE([foreign])

DIFXMESSAGE_REQUIRED_VERSION=2.2.0
DIFXMESSAGE_MAX_VERSION=7
FXCORR_REQUIRED_VERSION=2.2
DIFXIO_REQUIRED_VERSION=3.3.0
PKG_CHECK_MODULES(DIFXIO, difxio >= $DIFXIO_REQUIRED_VERSION)
PKG_CHECK_MODULES(FXCORR, fxcorr >= $FXCORR_REQUIRED_VERSION)
PKG_CHECK_MODULES(DIFXMESSAGE, [difxmessage >= $DIFXMESSAGE_REQUIRED_VERSION difxmessage < $DIFXMESSAGE_MAX_VERSION])


PKG_CHECK_MODULES(PLPLOT, plplot, [hasplplot=true], [hasplplot=false])
if test "$hasplplot" == "false"; then
  PKG_CHECK_MODULES(PLPLOT, plplotd, [hasplplot=true], [hasplplot=false])
fi
if test "$hasplplot" == "false"; then
  AC_MSG_NOTICE([PLPLOT was not found on your system.  Some utilities wont be built])
fi
AM_CONDITIONAL(WITH_PLPLOT, $hasplplot)


CXXFLAGS="${CXXFLAGS} ${DIFXIO_CFLAGS} ${DIFXMESSAGE_CFLAGS} ${FXCORR_CFLAGS} ${PLPLOT_CFLAGS}"
LIBS=" ${FXCORR_LIBS} ${DIFXIO_LIBS} ${DIFXMESSAGE_LIBS} ${PLPLOT_LIBS}"

AC_PROG_CXX
AC_PROG_CC

AC_OUTPUT([
	Makefile \
	src/Makefile \
	utilities/Makefile \
	transient_daemon/Makefile \
	transient_wrapper/Makefile \
])
