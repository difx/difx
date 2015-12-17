AM_CPPFLAGS = \
        -I$(top_srcdir)

LDADD = $(top_builddir)/difxio/libdifxio.la -lm

bin_PROGRAMS = \
	testdifxinput \
	testparsedifx \
	testparsevis \
	testtcal \
	pbgen \
	testephem \
	teststringarray

testdifxinput_SOURCES = \
	testdifxinput.c

testparsedifx_SOURCES = \
	testparsedifx.c

testparsevis_SOURCES  = \
	testparsevis.c

testtcal_SOURCES = \
	testtcal.c

testephem_SOURCES = \
	testephem.c

pbgen_SOURCES = \
	pbgen.c \
	pbgen.h \
	util.c \
	util.h

teststringarray_SOURCES  = \
	teststringarray.c

