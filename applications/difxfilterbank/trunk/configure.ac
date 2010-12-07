AC_INIT(src/difxfilterbank.cpp)

PACKAGE=difxfilterbank
VERSION=0.3

AM_INIT_AUTOMAKE($PACKAGE, $VERSION)

DIFXMESSAGE_REQUIRED_VERSION=2.0.1
DIFXMESSAGE_MAX_VERSION=7
FXCORR_REQUIRED_VERSION=2.0
DIFXIO_REQUIRED_VERSION=3.0.1
PKG_CHECK_MODULES(DIFXMESSAGE, [difxmessage >= $DIFXMESSAGE_REQUIRED_VERSION difxmessage < $DIFXMESSAGE_MAX_VERSION])
PKG_CHECK_MODULES(DIFXIO, difxio >= $DIFXIO_REQUIRED_VERSION)
PKG_CHECK_MODULES(FXCORR, fxcorr >= $FXCORR_REQUIRED_VERSION)
AC_SUBST(DIFXMESSAGE_CFLAGS)
AC_SUBST(DIFXMESSAGE_LIBS)
AC_SUBST(DIFXIO_CFLAGS)
AC_SUBST(DIFXIO_LIBS)
AC_SUBST(FXCORR_CFLAGS)
AC_SUBST(FXCORR_LIBS)

dnl **********************************************************
dnl **** Set path for Intel Performance Primitive library ****
dnl **********************************************************
PKG_CHECK_MODULES(IPP, ipp)

dnl OSX does not need libX11 for PGPLOT
if test "$OSTYPE" == "darwin"; then
          X11LIBS=""
else
          X11LIBS="-lX11"
fi

dnl **********************************************************
dnl **** Check PGPLOTDIR it set and only use it it is
dnl **********************************************************
if test "x$PGPLOTDIR" == "x"; then
   	  PGPLOTINC=""
  	  PGPLOTLIB=""
else
	  PGPLOTINC = "-I$(PGPLOTDIR)"
  	  PGPLOTLIB = "-L$(PGPLOTDIR)"
fi

AC_SUBST(X11LIBS)
AC_SUBST(PGPLOTINC)
AC_SUBST(PGPLOTLIB)
AC_SUBST(IPP_CFLAGS)
AC_SUBST(IPP_LIBS)
dnl **********************************************************

AC_PROG_CXX
AC_PROG_CC
AC_F77_LIBRARY_LDFLAGS

AC_OUTPUT(Makefile src/Makefile utilities/Makefile transient_daemon/Makefile transient_wrapper/Makefile)
