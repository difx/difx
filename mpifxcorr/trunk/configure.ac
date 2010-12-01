AC_INIT([mpifxcorr], [2.0.1], [Adam Deller <adeller@astro.swin.edu.au>])
PACKAGE_VERSION=AC_PACKAGE_VERSION
AC_SUBST([PACKAGE_VERSION])
AC_SUBST([LIBRARY_VERSION])
AM_INIT_AUTOMAKE
AM_CONFIG_HEADER(config.h)
AC_LANG_CPLUSPLUS
AC_PROG_CXX
AC_PROG_RANLIB
AC_C_BIGENDIAN


AC_ARG_ENABLE(quiet, [AS_HELP_STRING([--enable-quiet],[disable some verbiage])], [CFLAG_QUIET="-DQUIET"], [CFLAG_QUIET=" "])

dnl **********************************************************
dnl **** Set path for Intel Performance Primitive library ****
dnl **********************************************************

PKG_CHECK_MODULES(IPP, ipp)
dnl **********************************************************


dnl **********************************************************
dnl **** Find difxmessage library                         ****
dnl **********************************************************
dm_enabled=true
PKG_CHECK_MODULES(DIFXMESSAGE,
        [difxmessage >= 2.0.1 difxmessage < 7],
        [AC_DEFINE([HAVE_DIFXMESSAGE], 1, [1=has DIFXMESSAGE])],
        dm_enabled=false)
AM_CONDITIONAL(WITH_DIFXMESSAGE, $dm_enabled)

dnl **********************************************************
dnl **** Find mark5ipc library                            ****
dnl **********************************************************
m5ipc_enabled=true
PKG_CHECK_MODULES(MARK5IPC,
        [mark5ipc >= 2.0.1 mark5ipc < 7],
	[AC_DEFINE([HAVE_MARK5IPC], 1, [1=has MARK5IPC])],
	m5ipc_enabled=false)
AM_CONDITIONAL(WITH_MARK5IPC, $m5ipc_enabled)

dnl **********************************************************
dnl **** Look for StreamStor libraries                    ****
dnl **** cause native Mark5 to be built if found.         ****
dnl **********************************************************
ss_enabled=true
PKG_CHECK_EXISTS([streamstor >= 8.0],
	[ PKG_CHECK_MODULES(SS, streamstor) ],
	[ AC_MSG_NOTICE([Did not find STREAMSTOR in $PKG_CONFIG_PATH])
	  ss_enabled=false])
if test ${ss_enabled} = true; then
	CPPFLAGS="${CPPFLAGS} ${SS_CFLAGS}"
	CXXFLAGS="${CXXFLAGS} ${SS_CFLAGS}"
	AC_CHECK_HEADERS([xlrapi.h])
else
	SS_CFLAGS=""
	SS_LIBS=""
	echo 'No suitable streamstor.pc file was found so the native'
	echo 'Mark5 support will be suppressed.  See the documentation'
	echo 'if you are surprised by this.'
fi
AM_CONDITIONAL(WITH_SS, $ss_enabled)

dnl **********************************************************
dnl **** Find mark5access library                         ****
dnl **********************************************************
PKG_CHECK_MODULES(M5ACCESS, mark5access >= 1.4.2)
dnl **********************************************************


CXXFLAGS="${CXXFLAGS} ${M5ACCESS_CFLAGS} ${IPP_CFLAGS} ${DIFXMESSAGE_CFLAGS} ${MARK5IPC_CFLAGS} ${CFLAG_QUIET}"
LIBS="${M5ACCESS_LIBS} ${SS_LIBS} ${IPP_LIBS} ${DIFXMESSAGE_LIBS} ${MARK5IPC_LIBS}"

echo "CXXFLAGS = ${CXXFLAGS}"
echo "LIBS = ${LIBS}"
AC_OUTPUT(Makefile mpifxcorr.pc fxcorr.pc src/Makefile scripts/Makefile utils/Makefile)
