
if WITH_GSL
noinst_PROGRAMS = testpoly

testpoly_SOURCES = \
	poly.c \
	poly.h \
	testpoly.c

gsl_progs = \
	difxvmf
endif

bin_PROGRAMS = \
	$(gsl_progs) \
	calcif2 

calcif2_SOURCES = \
	calcif2.c \
	difxcalc.c \
	difxcalc.h \
	externaldelay.c \
	externaldelay.h \
	CALC_xdr.c \
	CALCServer.h \
	poly.c \
	poly.h \
	MATHCNST.H

if WITH_GSL
difxvmf_SOURCES = \
	poly.c \
	poly.h \
	vmf.c \
	vmf.h \
	vmf3.c \
	vmf3.h \
	timeutils.c \
	timeutils.h \
	wxdata.c \
	wxdata.h \
	difxvmf.c
endif
