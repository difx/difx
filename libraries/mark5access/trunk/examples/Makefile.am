AM_CPPFLAGS = -I$(top_srcdir)/mark5access -I$(srcdir)/.. $(CODIFIO_CFLAGS)
LDADD =    $(top_builddir)/mark5access/libmark5access.la $(CODIFIO_LIBS) -lm

if WITH_FFTW
fftw_programs = \
	m5pcal \
	m5spec \
	m5fb \
	m5subband \
	zerocorr 
m5subband_CFLAGS = $(FFTW3_CFLAGS) $(INCLUDES)
m5subband_LDADD = $(FFTW3_LIBS) $(LDADD) -lfftw3f
m5spec_CFLAGS = $(FFTW3_CFLAGS) $(INCLUDES)
m5spec_LDADD = $(FFTW3_LIBS) $(LDADD)
m5fb_CFLAGS = $(FFTW3_CFLAGS) $(INCLUDES)
m5fb_LDADD = $(FFTW3_LIBS) $(LDADD)
m5pcal_CFLAGS = $(FFTW3_CFLAGS) $(INCLUDES)
m5pcal_LDADD = $(FFTW3_LIBS) $(LDADD)
zerocorr_CFLAGS = $(FFTW3_CFLAGS) $(INCLUDES)
zerocorr_LDADD = $(FFTW3_LIBS) $(LDADD)
else
fftw_programs = 
endif

dist_bin_SCRIPTS = zerocorr_makeconfig.py

bin_PROGRAMS = \
	directory2filelist \
	fixmark5b \
	m5bstate \
	m5d \
	m5fold \
	m5timeseries \
	m5tsys \
	m5test \
	m5time \
	m5slice \
	m5findformats \
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

m5fb_SOURCES = \
	m5fb.c

m5test_SOURCES = \
	m5test.c

m5findformats_SOURCES = \
	m5findformats.c

test_mark5_stream_SOURCES = \
	test_mark5_stream.c

test_unpacker_SOURCES = \
	test_unpacker.c

m5subband_SOURCES = \
	m5subband.c

zerocorr_SOURCES = \
	zerocorr.c


install-exec-hook:
	mv $(DESTDIR)$(bindir)/zerocorr_makeconfig.py $(DESTDIR)$(bindir)/zerocorr_makeconfig
