AC_INIT()

AM_INIT_AUTOMAKE(CalcServer, 1.6, no-define)

AC_PROG_CC
AM_SANITY_CHECK

AC_LANG_C

AC_PROG_F77
AM_PROG_LIBTOOL

AC_CHECK_LIB(m, sin,,)

AC_OUTPUT(Makefile \
	startCalcServer \
	src/Makefile \
	calc9.1/Makefile \
	init.d/calcserver \
	data/Makefile
)
