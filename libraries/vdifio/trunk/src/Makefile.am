h_sources = \
	vdifio.h \
	vdif_mark6.h

c_sources = \
	vdifbuffer.c \
	vdiffile.c \
	vdifio.c \
	vdif_mark6.c \
	decode.c \
	vdifmux.c

library_includedir = $(includedir)
library_include_HEADERS = \
	$(h_sources)

lib_LTLIBRARIES = \
	libvdifio.la

libvdifio_la_SOURCES = \
	$(h_sources) \
	$(c_sources)

