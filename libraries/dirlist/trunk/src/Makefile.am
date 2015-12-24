h_sources = \
	parse.h \
	dirlist_exception.h \
	dirlist_parameter.h \
	dirlist_datum.h \
	dirlist_datum_mark5.h \
	dirlist_datum_mark6.h \
	dirlist.h \
	old_dirlist.h \
	old_filelist.h

cpp_sources = \
	parse.cpp \
	dirlist_parameter.cpp \
	dirlist_datum.cpp \
	dirlist_datum_mark5.cpp \
	dirlist.cpp \
	old_dirlist.cpp \
	old_filelist.cpp

library_includedir = $(includedir)/dirlist
library_include_HEADERS = $(h_sources)

lib_LTLIBRARIES = \
	libdirlist.la

libdirlist_la_SOURCES = $(h_sources) $(cpp_sources)
libdirlist_la_LDFLAGS = -version-info $(LIBRARY_VERSION)

