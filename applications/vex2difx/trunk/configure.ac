AC_INIT([vex2difx], [2.0.2], [Walter Brisken <wbrisken@nrao.edu>])
AM_CONFIG_HEADER([config.h])

AC_C_BIGENDIAN

AM_INIT_AUTOMAKE

AC_PROG_CC
AC_PROG_CPP
AC_PROG_LIBTOOL
AC_PROG_YACC
AM_PROG_LEX
AM_SANITY_CHECK
AC_PROG_CXX

AM_SANITY_CHECK

AC_CHECK_LIB(m, erf,,[AC_MSG_ERROR("need libm")])

PKG_CHECK_MODULES(DIFXIO, difxio >= 3.0.2)
PKG_CHECK_MODULES(DIFXMESSAGE, [difxmessage >= 2.0.2 difxmessage < 7])

AC_ARG_WITH(spice-root, [  --with-spice-root=ROOT
                            spice root is ROOT (optional).],
spice_root="$withval", spice_root="$SPICE_ROOT")
if test x"$spice_root" != "x"; then
	SPICE_CFLAGS="-I$spice_root/include"
	SPICE_LIBS="$spice_root/lib/cspice.a"
	spice_enabled=true
	echo "Spice enabled"
	AC_DEFINE([HAVE_SPICE], [1], [1=has spice library])
else
	SPICE_CFLAGS=""
	SPICE_LIBS=""
	spice_enabled=false
	echo "Spice disabled"
	AC_DEFINE([HAVE_SPICE], [0], [1=has spice library])
fi
AM_CONDITIONAL(HAVE_SPICE, $spice_enabled)

LIBS="${LIBS} ${DIFXIO_LIBS} ${SPICE_LIBS}"
CFLAGS="${CFLAGS} ${DIFXIO_CFLAGS} ${DIFXMESSAGE_CFLAGS} ${SPICE_CFLAGS}"
CXXFLAGS="${CFLAGS}"

AC_OUTPUT([ \
	Makefile \
	vex/Makefile \
	vex/vex.pc \
	src/Makefile \
	utilities/Makefile \
])
