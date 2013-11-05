h_sources = \
	vdifio.h

c_sources = \
	vdifbuffer.c \
	vdiffile.c \
	vdifio.c \
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

