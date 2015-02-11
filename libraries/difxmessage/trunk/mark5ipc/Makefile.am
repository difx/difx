c_sources = \
	mark5lock.c

library_includedir = $(includedir)/mark5ipc
library_include_HEADERS = $(h_sources)

AM_CPPFLAGS = -I$(top_srcdir)

lib_LTLIBRARIES = \
	libmark5ipc.la

libmark5ipc_la_SOURCES = $(h_sources) $(c_sources)
libmark5ipc_la_LDFLAGS = -version-info $(LIBRARY_VERSION)

