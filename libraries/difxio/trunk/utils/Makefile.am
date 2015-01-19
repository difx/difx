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
	difxcalculator

calcderiv_SOURCES = \
	calcderiv.c

difxcalculator_SOURCES = \
	difxcalculator.c

if WITH_FFTW
psrflag_SOURCES = \
	psrflag.c
endif
