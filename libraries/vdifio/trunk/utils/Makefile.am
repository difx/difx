INCLUDES = \
	-I$(top_srcdir)/src

LDADD = \
	$(top_builddir)/src/libvdifio.a

bin_PROGRAMS = \
	stripVDIF \
	captureUDPVDIF \
	multi2singlethreadVDIF \
	padVDIF \
	cleanVDIF \
	fakemultiVDIF \
	extractSingleVDIFThread \
	countVDIFPackets \
	printVDIF 

stripVDIF_SOURCES = \
	stripVDIF.c

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
