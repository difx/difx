h_sources = \
	dirlist.h

cpp_sources = \
	dirlist.cpp

library_includedir = $(includedir)/dirlist
library_include_HEADERS = $(h_sources)

lib_LTLIBRARIES = \
	libdirlist.la

libdirlist_la_SOURCES = $(h_sources) $(cpp_sources)
libdirlist_la_LDFLAGS = -version-info $(LIBRARY_VERSION)

