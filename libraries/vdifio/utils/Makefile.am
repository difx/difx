
AM_CPPFLAGS  = \
	-I$(top_srcdir)/src -D_FILE_OFFSET_BITS=64

LDADD = \
	$(top_builddir)/src/libvdifio.la

bin_PROGRAMS = \
	stripVDIF \
	checkVDIF \
	captureUDPVDIF \
	multi2singlethreadVDIF \
	padVDIF \
	cleanVDIF \
	reorderVDIF \
	fakemultiVDIF \
	extractSingleVDIFThread \
	extractVDIFThreads \
	filterVDIF \
	countVDIFPackets \
	printVDIF \
	printVDIFgaps \
	printVDIFheader \
	peekVDIF \
	searchVDIF \
	splitVDIFbygap \
	vdif2to8 \
	vdifChanSelect \
	vdifSplitThreads \
	vmux \
	vsum

if USE_MARK6SG
  bin_PROGRAMS += mk6vmux
endif

if USE_IPP 
if !OLD_IPP
  bin_PROGRAMS += generateVDIF
  bin_PROGRAMS += vdifLinear2circular	
endif
  bin_PROGRAMS += vdifSwapSideband
endif

noinst_PROGRAMS = \
	testcornerturners

dist_bin_SCRIPTS = \
	vdifbstate \
	vdiffold \
	vdifd \
	vdifspec

testcornerturners_SOURCES = \
	testcornerturners.c

mk6vmux_SOURCES = \
	mk6vmux.c

stripVDIF_SOURCES = \
	stripVDIF.c

checkVDIF_SOURCES = \
	checkVDIF.c

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

extractVDIFThreads_SOURCES = \
	extractVDIFThreads.c

filterVDIF_SOURCES = \
	filterVDIF.c

reorderVDIF_SOURCES = \
	reorderVDIF.c

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

splitVDIFbygap_SOURCES = \
	splitVDIFbygap.c

vdif2to8_SOURCES = \
	vdif2to8.c

vdifChanSelect_SOURCES = \
	vdifChanSelect.c

vdifSplitThreads_SOURCES = \
	vdifSplitThreads.c

generateVDIF_SOURCES = \
	generateVDIF.c

vdifSwapSideband_SOURCES = \
	vdifSwapSideband.c

vdifLinear2circular_LDADD = $(IPP_LIBS) $(LDADD)

vdifSwapSideband_LDADD = $(IPP_LIBS) $(LDADD)
if USE_CODIFIO
  generateVDIF_LDADD = $(IPP_LIBS) $(LDADD) $(CODIFIO_LIBS)
else
  generateVDIF_LDADD = $(IPP_LIBS) $(LDADD)
endif

vmux_SOURCES = \
	vmux.c

vsum_SOURCES = \
	vsum.c

