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
if test "$OSTYPE" == "darwin"; then
        ipp_libname="Libraries"
else
        ipp_libname="sharedlib"
fi

AC_ARG_WITH(ipp-root, [AS_HELP_STRING([--with-ipp-root=ROOT],[ipp root is ROOT (optional).  Use env variable IPPROOT otherwise.])],
ipp_root="$withval", ipp_root="")
if test x"$ipp_root" != "x"; then
        IPP_CFLAGS="-I$ipp_root/include"
        IPP_LIBS="-L$ipp_root/$ipp_libname"
else
        IPP_CFLAGS="-I$IPPROOT/include"
        IPP_LIBS="-L$IPPROOT/$ipp_libname"
fi

if test x"$IPPLINKLIBS" != x; then
        IPP_LIBS=$IPP_LIBS" $IPPLINKLIBS"
else
        if test "$DIFXBITS" == "32"; then
                IPP_LIBS=$IPP_LIBS" -lipps -lguide -lippvm -lippcore"
        elif test "$DIFXBITS" == "64"; then
                IPP_LIBS=$IPP_LIBS" -lippsem64t -lguide -lippvmem64t -lippcoreem64t"
        else
                IPP_LIBS=$IPP_LIBS" -lipps -lguide -lippvm"
                echo "32/64 bit unspecified - assuming 32 bit for IPP"
        fi
fi
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
AC_CHECK_HEADER([xlrapi.h],
		[AC_DEFINE([HAVE_XLRAPI_H], 1, [1=has SS])],
		[ss_enabled=false])
if test ${ss_enabled} = true; then
	SS_LIBS="-lwdapi921 -lssapi -lrt"
else
	SS_LIBS=""
fi
AM_CONDITIONAL(WITH_SS, $ss_enabled)
dnl **********************************************************


dnl **********************************************************
dnl **** Find mark5access library                         ****
dnl **********************************************************
PKG_CHECK_MODULES(M5ACCESS, mark5access >= 1.4.2)
dnl **********************************************************


CXXFLAGS="${CXXFLAGS} ${M5ACCESS_CFLAGS} ${IPP_CFLAGS} ${DIFXMESSAGE_CFLAGS} ${MARK5IPC_CFLAGS} ${CFLAG_QUIET}"
LIBS="${M5ACCESS_LIBS} ${SS_LIBS} ${IPP_LIBS} ${DIFXMESSAGE_LIBS} ${MARK5IPC_LIBS}"

echo "CXXFLAGS = ${CXXFLAGS}"
echo "LIBS = ${LIBS}"
AC_OUTPUT(Makefile mpifxcorr.pc fxcorr.pc src/Makefile scripts/Makefile)
