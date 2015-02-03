AC_INIT([SniffPlots], [0.993], [Craig Walker <cwalker@nrao.edu>])

AM_INIT_AUTOMAKE([foreign])

AC_PROG_F77
AC_LANG_FORTRAN77

PKG_CHECK_MODULES(RCWLIB, rcwlib >= 0.99)
PKG_CHECK_MODULES(CITLIB, citlib >= 0.99)
PKG_CHECK_MODULES(LIBPNG, libpng)

LIBS="${LIBS} ${RCWLIB_LIBS} ${CITLIB_LIBS}  -L${PGPLOT_DIR} -lpgplot ${LIBPNG_LIBS} -lX11"

AC_OUTPUT([ \
	Makefile \
	clockhist/Makefile \
	plotapd/Makefile \
	plotbp/Makefile \
	plotgps/Makefile \
	plotwt/Makefile \
	utilities/Makefile \
])
