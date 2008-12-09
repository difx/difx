AC_INIT([mpifxcorr], [1.5], [Adam Deller <adeller@astro.swin.edu.au>])
PACKAGE_VERSION=AC_PACKAGE_VERSION
AC_SUBST([PACKAGE_VERSION])
AC_SUBST([LIBRARY_VERSION])
AM_INIT_AUTOMAKE
AM_CONFIG_HEADER(config.h)
AC_LANG_CPLUSPLUS
AC_PROG_CXX
AC_PROG_RANLIB

AC_ARG_ENABLE(quiet, ["disable some verbiage"], [CFLAG_QUIET="-DQUIET"], [CFLAG_QUIET=" "])

dnl **********************************************************
dnl **** Set path for Intel Performance Primitive library ****
dnl **********************************************************
AC_ARG_WITH(ipp-root, [  --with-ipp-root=ROOT
                          ipp root is ROOT (optional).  Use env variable
                          IPPROOT otherwise.],
ipp_root="$withval", ipp_root="")
if test x"$ipp_root" != "x"; then
        IPP_CFLAGS="-I$ipp_root/include"
        IPP_LIBS="-L$ipp_root/sharedlib"
else
        IPP_CFLAGS="-I$IPPROOT/include"
        IPP_LIBS="-L$IPPROOT/sharedlib"
fi


if test "$DIFXBITS" == "32"; then
        IPP_LIBS=$IPP_LIBS" -lipps -lguide -lippvm -lippcore"
elif test "$DIFXBITS" == "64"; then
        IPP_LIBS=$IPP_LIBS" -lippsem64t -lguide -lippvmem64t -liomp5 -lippcoreem64t"
else
        IPP_LIBS=$IPP_LIBS" -lipps -lguide -lippvm"
        echo "32/64 bit unspecified - assuming 32 bit for IPP"
fi
dnl **********************************************************


dnl **********************************************************
dnl **** Find difxmessage library                         ****
dnl **********************************************************
dm_enabled=true
PKG_CHECK_MODULES(DIFXMESSAGE,
        difxmessage,
	[AC_DEFINE([HAVE_DIFXMESSAGE], 1, [1=has DIFXMESSAGE])],
	dm_enabled=false)
AM_CONDITIONAL(WITH_DIFXMESSAGE, $dm_enabled)

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
dnl **** Look for RPFITS                                  ****
dnl **********************************************************
rpfits_enabled=true
PWD=`pwd`
AC_CHECK_HEADER([$DIFXROOT/include/RPFITS.h],
		[AC_DEFINE([HAVE_RPFITS], 1, [1=has RPFITS])],
		[rpfits_enabled=false])
if test ${rpfits_enabled} = true; then
	RPFITS_CFLAGS="-I$DIFXROOT/include"
	RPFITS_LIBS=" -lrpfits -lg2c"
else
RPFITS_CFLAGS=""
RPFITS_LIBS=""
fi
dnl **********************************************************


dnl **********************************************************
dnl **** Find mark5access library                         ****
dnl **********************************************************
PKG_CHECK_MODULES(M5ACCESS, mark5access)
dnl **********************************************************


CXXFLAGS="${CXXFLAGS} ${M5ACCESS_CFLAGS} ${IPP_CFLAGS} ${RPFITS_CFLAGS} ${DIFXMESSAGE_CFLAGS} ${CFLAG_QUIET}"
LIBS="${M5ACCESS_LIBS} ${SS_LIBS} ${IPP_LIBS} ${RPFITS_LIBS} ${DIFXMESSAGE_LIBS}"

echo "CXXFLAGS = ${CXXFLAGS}"
echo "LIBS = ${LIBS}"
AC_OUTPUT(Makefile mpifxcorr.pc fxcorr.pc src/Makefile scripts/Makefile)
