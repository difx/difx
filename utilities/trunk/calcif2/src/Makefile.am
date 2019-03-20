bin_PROGRAMS = calcif2 difxvmf

if WITH_GSL
noinst_PROGRAMS = testpoly

testpoly_SOURCES = \
	poly.c \
	poly.h \
	testpoly.c
endif

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

difxvmf_SOURCES = \
	poly.c \
	poly.h \
	vmf.c \
	vmf.h \
	vmf3.c \
	vmf3.h \
	timeutils.c \
	timeutils.h \
	difxvmf.c
