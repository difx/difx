AC_PREREQ(2.50)
AC_INIT([DIFX2PROFILE], [0.1], [difxusers at googlegroups.com])
PACKAGE_VERSION=AC_PACKAGE_VERSION
AC_SUBST([PACKAGE_VERSION])
AC_SUBST([LIBRARY_VERSION])

AM_INIT_AUTOMAKE

AC_LANG_CPLUSPLUS
AC_CONFIG_SRCDIR([src/difx2profile.h])
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
PKG_CHECK_MODULES(FXCORR, fxcorr >= 1.5)
dm_enabled=true
PKG_CHECK_MODULES(DIFXMESSAGE, difxmessage >= 0.4,
                  [AC_DEFINE([HAVE_DIFXMESSAGE], 1, [1=has DIFXMESSAGE])],
                  dm_enabled=false)
AM_CONDITIONAL(WITH_DIFXMESSAGE, $dm_enabled)

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

CXXFLAGS="${CXXFLAGS} ${IPP_CFLAGS} ${DIFXIO_CFLAGS} ${DIFXMESSAGE_CFLAGS} ${FXCORR_CFLAGS}"
LIBS="${IPP_LIBS} ${DIFXIO_LIBS} ${DIFXMESSAGE_LIBS} ${FXCORR_LIBS} -lpthread"

AC_CONFIG_FILES([Makefile src/Makefile])
AC_OUTPUT
