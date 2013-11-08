INCLUDES = -I$(top_srcdir)/mark5access -I$(srcdir)/..
LDADD =    $(top_builddir)/mark5access/libmark5access.la -lm

if WITH_FFTW
fftw_programs = \
	m5pcal \
	m5spec \
	m5tone \
	zerocorr
m5spec_CFLAGS = $(FFTW3_CFLAGS) $(INCLUDES)
m5spec_LDADD = $(FFTW3_LIBS) $(LDADD)
m5pcal_CFLAGS = $(FFTW3_CFLAGS) $(INCLUDES)
m5pcal_LDADD = $(FFTW3_LIBS) $(LDADD)
zerocorr_CFLAGS = $(FFTW3_CFLAGS) $(INCLUDES)
zerocorr_LDADD = $(FFTW3_LIBS) $(LDADD)
else
fftw_programs = 
endif

m5tone_CFLAGS =  -I/cluster/pgplot 
m5tone_LDADD =  $(LDADD)  -lfftw3 -lgfortran -lX11 -lpng /cluster/pgplot/libcpgplot.a /cluster/pgplot/libpgplot.a

bin_PROGRAMS = \
	directory2filelist \
	fixmark5b \
	m5bstate \
	m5bsum \
	m5d \
	m5fold \
	m5timeseries \
	m5tsys \
	m5test \
	m5time \
	m5slice \
	m5findformats \
	m5tone \
	test5b \
	test_mark5_stream \
	test_unpacker \
	$(fftw_programs)

directory2filelist_SOURCES = \
	directory2filelist.c

fixmark5b_SOURCES = \
	fixmark5b.c

m5bstate_SOURCES = \
	m5bstate.c

m5bsum_SOURCES = \
	m5bsum.c

m5d_SOURCES = \
	m5d.c

test5b_SOURCES = \
	test5b.c

m5tsys_SOURCES = \
	m5tsys.c

m5pcal_SOURCES = \
	m5pcal.c

m5spec_SOURCES = \
	m5spec.c

m5test_SOURCES = \
	m5test.c

m5findformats_SOURCES = \
	m5findformats.c

m5tone_SOURCES = \
	m5tone.c	

test_mark5_stream_SOURCES = \
	test_mark5_stream.c

test_unpacker_SOURCES = \
	test_unpacker.c

zerocorr_SOURCES = \
	zerocorr.c
