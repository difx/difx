h_sources = \
	difx_input.h \
	parsedifx.h \
	parsevis.h
c_sources = \
	difx_input.c \
	difx_input_merge.c \
	difx_input_freq.c \
	difx_input_antenna.c \
	difx_input_baseline.c \
	difx_input_datastream.c \
	parsedifx.c \
	parsevis.c

library_includedir = $(includedir)/difxio
library_include_HEADERS = $(h_sources)

INCLUDES = -I$(top_srcdir)

lib_LTLIBRARIES = \
	libdifxio.la

libdifxio_la_SOURCES = $(h_sources) $(c_sources)
libdifxio_la_LDFLAGS = -version-info $(LIBRARY_VERSION)

