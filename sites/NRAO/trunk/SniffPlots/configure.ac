AC_INIT([SniffPlots], [0.995], [Walter Brisken <wbrisken@nrao.edu> and Craig Walker <cwalker@nrao.edu>])

AM_INIT_AUTOMAKE([foreign])

AC_PROG_F77
AC_LANG_FORTRAN77

PKG_CHECK_MODULES(RCWLIB, rcwlib >= 0.99)
PKG_CHECK_MODULES(CITLIB, citlib >= 0.99)

LIBS="${LIBS} ${RCWLIB_LIBS} ${CITLIB_LIBS}  -L${PGPLOT_DIR} -lpgplot -lX11"

AC_OUTPUT([ \
	Makefile \
	clockhist/Makefile \
	plotapd/Makefile \
	plotbp/Makefile \
	plotgps/Makefile \
	plotwt/Makefile \
	utilities/Makefile \
])
