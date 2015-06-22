AM_CPPFLAGS = \
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
	tabulatedelays

calcderiv_SOURCES = \
	calcderiv.c

difxcalculator_SOURCES = \
	difxcalculator.c

tabulatedelays_SOURCES = \
	tabulatedelays.c

if WITH_FFTW
psrflag_SOURCES = \
	psrflag.c
endif
