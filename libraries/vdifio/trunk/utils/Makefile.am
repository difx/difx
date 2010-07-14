INCLUDES = \
	-I$(top_srcdir)/src

LDADD = \
	$(top_builddir)/src/libvdifio.a

bin_PROGRAMS = \
	stripVDIF \
	captureUDPVDIF \
	padVDIF \
	printVDIF 

stripVDIF_SOURCES = \
	stripVDIF.c

captureUDPVDIF_SOURCES = \
	captureUDPVDIF.c

captureUDPVDIF_LDFLAGS = \
	-pthread

padVDIF_SOURCES = \
	padVDIF.c

printVDIF_SOURCES = \
	printVDIF.c
