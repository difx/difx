h_sources = mark6_sg_utils.h mark6_sg_vfs.h
c_sources = mark6_sg_utils.c mark6_sg_vfs.c

library_includedir = $(includedir)/mark6sg
library_include_HEADERS = $(h_sources)

INCLUDES = -I$(top_srcdir)/mark6sg

lib_LTLIBRARIES = libmark6sg.la

libmark6sg_la_SOURCES = $(h_sources) $(c_sources)
libmark6sg_la_LDFLAGS = -version-info $(LIBRARY_VERSION) -pthread -L$(top_builddir)/jsmn/
libmark6sg_la_LIBADD = $(top_builddir)/jsmn/libjsmn.la
libmark6sg_la_CFLAGS = -Wall -I$(top_srcdir)/jsmn/ -pthread
