
INCLUDES = -I$(top_srcdir)/mark5access
LDADD =    $(top_builddir)/mark5access/libmark5access.la -lm

if WITH_FFTW
fftw_programs = m5spec
m5spec_CFLAGS = $(FFTW3_CFLAGS) $(INCLUDES)
m5spec_LDADD = $(FFTW3_LIBS) $(LDADD)
else
fftw_programs = 
endif

bin_PROGRAMS = \
	test_mark5_stream \
	test_unpacker \
	m5test \
	m5fold \
	m5d \
	$(fftw_programs)

test_mark5_stream_SOURCES = \
	test_mark5_stream.c

test_unpacker_SOURCES = \
	test_unpacker.c

m5d_SOURCES = \
	m5d.c

m5test_SOURCES = \
	m5test.c

m5spec_SOURCES = \
	m5spec.c
