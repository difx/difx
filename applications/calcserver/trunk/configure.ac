AC_INIT()

AM_INIT_AUTOMAKE(CalcServer, 1.6, no-define)

AC_PROG_CC
AM_SANITY_CHECK

AC_LANG_C

AC_PROG_F77(gfortran)
AM_PROG_LIBTOOL

AC_CHECK_LIB(m, sin,,)

# rpc functionality was dropped from some flavors of linux;
# tirpc provides the same API
PKG_CHECK_MODULES(TIRPC, libtirpc)
CPPFLAGS="${CPPFLAGS} ${TIRPC_CFLAGS}"
LIBS="${LIBS} ${TIRPC_LIBS}"

AC_OUTPUT(Makefile \
	startCalcServer \
	src/Makefile \
	calc9.1/Makefile \
	init.d/calcserver \
	data/Makefile
)
