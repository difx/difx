h_sources = mark6_sg_utils.h mark6_sg_vfs.h mark6_sg_format.h mark6_sg_vfs_impl.h mark6_sg_defines.h
c_sources = mark6_sg_utils.c mark6_sg_vfs.c

library_includedir = $(includedir)/mark6sg
library_include_HEADERS = $(h_sources)

lib_LTLIBRARIES = libmark6sg.la

libmark6sg_la_SOURCES = $(h_sources) $(c_sources)
libmark6sg_la_LDFLAGS = -version-info $(LIBRARY_VERSION) -L$(top_builddir)/jsmn/
libmark6sg_la_LIBADD = $(top_builddir)/jsmn/libjsmn.la
libmark6sg_la_CFLAGS = -Wall -I$(top_srcdir)/jsmn/ -I$(top_srcdir) -I$(top_srcdir)/mark6sg/
