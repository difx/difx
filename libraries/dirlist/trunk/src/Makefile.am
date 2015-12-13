h_sources = \
	parse.h \
	dirlist_exception.h \
	dirlist_parameter.h \
	dirlist_datum.h \
	dirlist.h

cpp_sources = \
	parse.cpp \
	dirlist_parameter.cpp \
	dirlist_datum.cpp \
	dirlist.cpp

library_includedir = $(includedir)/dirlist
library_include_HEADERS = $(h_sources)

lib_LTLIBRARIES = \
	libdirlist.la

libdirlist_la_SOURCES = $(h_sources) $(cpp_sources)
libdirlist_la_LDFLAGS = -version-info $(LIBRARY_VERSION)

