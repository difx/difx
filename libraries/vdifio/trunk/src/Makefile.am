
if USE_MARK6SG
mark6sg_h_sources = mark6gather_vdif.h
mark6sg_c_sources = mark6gather_vdif.c
else
mark6sg_h_sources =
mark6sg_c_sources =
endif


sources = \
	$(mark6sg_c_sources) \
	cornerturners.c \
	dateutils.c \
	dateutils.h \
	vdifbuffer.c \
	vdiffile.c \
	vdiffilereader.c \
	vdifio.c \
	vdifio.h \
	vdifmux.c

includeheaders = \
	$(mark6sg_h_sources) \
	vdifio.h

library_includedir = $(includedir)
library_include_HEADERS = $(includeheaders)

lib_LTLIBRARIES = \
	libvdifio.la

libvdifio_la_SOURCES = \
	$(sources)

