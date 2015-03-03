AC_INIT([vex2difx], [2.5.0], [Walter Brisken <wbrisken@nrao.edu>])
AM_CONFIG_HEADER([config.h])

AC_C_BIGENDIAN
 
AM_INIT_AUTOMAKE

AC_PROG_CC
AC_PROG_F77
AC_PROG_CPP
AC_PROG_LIBTOOL
AC_PROG_YACC
AM_PROG_LEX
AM_SANITY_CHECK
AC_PROG_CXX

AM_SANITY_CHECK

AC_CHECK_LIB(m, erf,,[AC_MSG_ERROR("need libm")])

#AC_CONFIG_MACRO_DIR([m4])
 
PKG_CHECK_MODULES(DIFXIO, difxio >= 3.4.0)
PKG_CHECK_MODULES(DIFXMESSAGE, [difxmessage >= 2.4.0 difxmessage < 7])

LIBS="${LEXLIB} ${LIBS} ${DIFXIO_LIBS}"
CFLAGS="${CFLAGS} ${DIFXIO_CFLAGS} ${DIFXMESSAGE_CFLAGS}"
CXXFLAGS="${CFLAGS}"

AC_OUTPUT([ \
	Makefile \
	vex/Makefile \
	vex/vex.pc \
	src/Makefile \
	utilities/Makefile \
])
