if WITH_MARK6SG
mark6sg_sources = \
	vdifmark6sg.c
mark6sg_includeheaders = \
	vdifmark6sg.h
else
mark6sg_sources =
mark6sg_includeheaders =
endif

sources = $(mark6sg_sources) \
	cornerturners.c \
	dateutils.c \
	dateutils.h \
	decode.c \
	vdifbuffer.c \
	vdiffile.c \
	vdifio.c \
	vdifio.h \
	vdifmark6.c \
	vdifmark6.h \
	vdifmark6mux.c \
	vdifmux.c

includeheaders = \
	vdifio.h

library_includedir = $(includedir)
library_include_HEADERS = $(includeheaders) $(mark6sg_includeheaders)

lib_LTLIBRARIES = \
	libvdifio.la

libvdifio_la_SOURCES = \
	$(sources)

