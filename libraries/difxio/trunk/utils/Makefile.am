INCLUDES = \
        -I$(top_srcdir)/difxio

LDADD = $(top_builddir)/difxio/libdifxio.la -lm

if WITH_FFTW
fftw_progs = \
	psrflag
endif

bin_PROGRAMS = \
	$(fftw_progs) \
	calcderiv \
	difxcalculator \
	fitdelay \
	fitxyz

dist_bin_SCRIPTS = \
	convertdelay

calcderiv_SOURCES = \
	calcderiv.c

difxcalculator_SOURCES = \
	difxcalculator.c

fitdelay_SOURCES = \
	delayderivatives.c \
	delayderivatives.h \
	fitdelay.c

fitxyz_SOURCES = \
	fitxyz.c
	
if WITH_FFTW
psrflag_SOURCES = \
	psrflag.c
endif
