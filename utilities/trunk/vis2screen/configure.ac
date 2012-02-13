AC_INIT(printDiFX.cpp)

PACKAGE=printDiFX
VERSION=0.2

AM_INIT_AUTOMAKE($PACKAGE, $VERSION)

FXCORR_REQUIRED_VERSION=2.0.1
PKG_CHECK_MODULES(FXCORR, fxcorr >= $FXCORR_REQUIRED_VERSION)
AC_SUBST(FXCORR_CFLAGS)
AC_SUBST(FXCORR_LIBS)

DIFXMESSAGE_REQUIRED_VERSION=2.0.1
PKG_CHECK_MODULES(DIFXMESSAGE, difxmessage >= $DIFXMESSAGE_REQUIRED_VERSION)
AC_SUBST(DIFXMESSAGE_CFLAGS)
AC_SUBST(DIFXMESSAGE_LIBS)

dnl **********************************************************
dnl **** Set path for Intel Performance Primitive library ****
dnl **********************************************************
PKG_CHECK_MODULES(IPP, ipp)

AC_SUBST(IPP_CFLAGS)
AC_SUBST(IPP_LIBS)
dnl **********************************************************

AC_PROG_CXX

AC_OUTPUT(Makefile)
