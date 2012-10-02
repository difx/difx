INCLUDES = \
        -I$(top_srcdir)

LDADD = $(top_builddir)/difxio/libdifxio.la -lm

bin_PROGRAMS = \
	testdifxinput \
	testparsedifx \
	testparsevis \
	testtcal \
	pbgen \
	teststringarray

testdifxinput_SOURCES = \
	testdifxinput.c
	
testparsedifx_SOURCES = \
	testparsedifx.c
	
testparsevis_SOURCES  = \
	testparsevis.c

testtcal_SOURCES = \
	testtcal.c

pbgen_SOURCES = \
	pbgen.c \
	util.c \
	util.h

teststringarray_SOURCES  = \
	teststringarray.c

