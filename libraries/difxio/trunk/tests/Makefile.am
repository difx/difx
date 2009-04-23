INCLUDES = \
        -I$(top_srcdir)/difxio

LDADD = $(top_builddir)/difxio/libdifxio.la -lm

bin_PROGRAMS = \
	testdifxinput \
	testparsedifx \
	testparsevis

testdifxinput_SOURCES = \
	testdifxinput.c
	
testparsedifx_SOURCES = \
	testparsedifx.c
	
testparsevis_SOURCES  = \
	testparsevis.c

