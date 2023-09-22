bin_PROGRAMS = CalcServer checkCalcServer
CalcServer_SOURCES = \
	CALC5Server.c \
	CALC_xdr.c \
	cvrtuc.c \
	ingdec.c \
	ingra.c \
	iword.c \
	mjd2str.c \
	sindex.c \
	time2str.c
CalcServer_LDADD = \
	$(top_builddir)/calc9.1/libCalc.la
checkCalcServer_SOURCES = \
	checkcalc.c \
	CALC_xdr.c
EXTRA_DIST = \
	CALCServer.h \
	MATHCNST.H \
	ocean.h \
	STDDEFS.H \
	vlb.h
	
