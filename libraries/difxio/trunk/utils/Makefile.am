AM_CPPFLAGS = \
        -I$(top_srcdir)/difxio

LDADD = $(top_builddir)/difxio/libdifxio.la -lm

if WITH_FFTW
fftw_progs = \
	psrflag
endif

if WITH_GSL
gsl_progs = \
	reducepoly
endif

bin_PROGRAMS = \
	$(fftw_progs) \
	$(gsl_progs) \
	avgDiFX \
	calcderiv \
	computetotals \
	difxcalculator \
	difxfilelist \
	tabulatedelays

avgDiFX_SOURCES = \
	avgDiFX.c

calcderiv_SOURCES = \
	calcderiv.c

computetotals_SOURCES = \
	computetotals.c

difxcalculator_SOURCES = \
	difxcalculator.c

difxfilelist_SOURCES = \
	difxfilelist.c

if WITH_GSL
reducepoly_SOURCES = \
	reducepoly.c
endif

tabulatedelays_SOURCES = \
	tabulatedelays.c

if WITH_FFTW
psrflag_SOURCES = \
	psrflag.c
endif
