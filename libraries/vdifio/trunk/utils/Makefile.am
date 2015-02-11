AM_CPPFLAGS  = \
	-I$(top_srcdir)/src -D_FILE_OFFSET_BITS=64

LDADD = \
	$(top_builddir)/src/libvdifio.la

bin_PROGRAMS = \
	stripVDIF \
	captureUDPVDIF \
	multi2singlethreadVDIF \
	padVDIF \
	cleanVDIF \
	fakemultiVDIF \
	extractSingleVDIFThread \
	extractVDIFThreads \
	countVDIFPackets \
	printVDIF \
	printVDIFgaps \
	printVDIFheader \
	peekVDIF \
	searchVDIF \
	vdif2to8 \
	vmux \
	vsum

noinst_PROGRAMS = \
	testcornerturners

dist_bin_SCRIPTS = \
	vdifbstate \
	vdiffold \
	vdifd \
	vdifspec

testcornerturners_SOURCES = \
	testcornerturners.c

stripVDIF_SOURCES = \
	stripVDIF.c

peekVDIF_SOURCES = \
	peekVDIF.c

captureUDPVDIF_SOURCES = \
	captureUDPVDIF.c

captureUDPVDIF_LDFLAGS = \
	-pthread

multi2singlethreadVDIF_SOURCES = \
	multi2singlethreadVDIF.c

countVDIFPackets_SOURCES = \
	countVDIFPackets.c

padVDIF_SOURCES = \
	padVDIF.c

extractSingleVDIFThread_SOURCES = \
	extractSingleVDIFThread.c

cleanVDIF_SOURCES = \
	cleanVDIF.c

fakemultiVDIF_SOURCES = \
	fakemultiVDIF.c

printVDIF_SOURCES = \
	printVDIF.c

printVDIFgaps_SOURCES = \
	printVDIFgaps.c

printVDIFheader_SOURCES = \
	printVDIFheader.c

searchVDIF_SOURCES = \
	searchVDIF.c

vdif2to8_SOURCES = \
	vdif2to8.c

vmux_SOURCES = \
	vmux.c

vsum_SOURCES = \
	vsum.c
