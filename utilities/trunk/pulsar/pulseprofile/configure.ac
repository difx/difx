AC_PREREQ(2.50)
AC_INIT([PULSEPROFILE], [0.1], [difxusers at googlegroups.com])
PACKAGE_VERSION=AC_PACKAGE_VERSION
AC_SUBST([PACKAGE_VERSION])
AC_SUBST([LIBRARY_VERSION])

AM_INIT_AUTOMAKE

AC_CONFIG_SRCDIR([src/pulseprofile.h])
AM_CONFIG_HEADER([config.h])

# Checks for programs.
AC_PROG_CXX
AC_PROG_RANLIB

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
        IPP_LIBS=$IPP_LIBS" -lippsem64t -lguide -lippvmem64t -lippcoreem64t"
else
        IPP_LIBS=$IPP_LIBS" -lipps -lguide -lippvm"
        echo "32/64 bit unspecified - assuming 32 bit for IPP"
fi
dnl **********************************************************

# Checks for libraries.
PKG_CHECK_MODULES(DIFXIO, difxio >= 2.9)
PKG_CHECK_MODULES(FXCORR, mpifxcorr >= 1.5)
PKG_CHECK_MODULES(DIFXMESSAGE, difxmessage >= 0.5)
PKG_CHECK_MODULES(M5ACCESS, mark5access >= 1.3.1)

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

CXXFLAGS="${CXXFLAGS} ${IPP_CFLAGS} ${DIFXMESSAGE_CFLAGS} ${DIFXIO_CFLAGS} ${FXCORR_CFLAGS} ${M5ACCESS_CFLAGS}"
LIBS="${IPP_LIBS} ${DIFXMESSAGE_LIBS} ${DIFXIO_LIBS} ${FXCORR_LIBS} ${M5ACCESS_LIBS}"

AC_CONFIG_FILES([Makefile src/Makefile])
AC_OUTPUT
