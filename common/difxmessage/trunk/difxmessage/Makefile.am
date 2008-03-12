h_sources = \
	multicast.h
c_source = \
	multicast.c

library_includedir = $(includedir)/difxmessage
library_include_HEADERS = $(h_sources)

INCLUDES = -I$(top_srcdir)

lib_LTLIBRARIES = \
	libdifxmessage.la

libdifxmessage_la_SOURCES = $(h_sources) $(c_sources)
libdifxmessage_la_LDFLAGS = -version-info $(LIBRARY_VERSION)

