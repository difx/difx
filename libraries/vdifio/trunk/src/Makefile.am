h_sources = \
	vdifio.h \
	vdifmark6.h

c_sources = \
	cornerturners.c \
	decode.c \
	vdifbuffer.c \
	vdiffile.c \
	vdifio.c \
	vdifmark6.c \
	vdifmark6mux.c \
	vdifmux.c

library_includedir = $(includedir)
library_include_HEADERS = \
	$(h_sources)

lib_LTLIBRARIES = \
	libvdifio.la

libvdifio_la_SOURCES = \
	$(h_sources) \
	$(c_sources)

