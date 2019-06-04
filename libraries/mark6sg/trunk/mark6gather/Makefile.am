h_sources = mark6gather.h
c_sources = mark6gather.c

library_includedir = $(includedir)/mark6sg
library_include_HEADERS = $(h_sources)

AM_CPPFLAGS = -I$(top_srcdir)/mark6sg

lib_LTLIBRARIES = libmark6gather.la

libmark6gather_la_SOURCES = $(h_sources) $(c_sources)
libmark6gather_la_LDFLAGS = -version-info $(LIBRARY_VERSION)

LDADD = \
	libmark6gather.la

bin_PROGRAMS = \
	mk6ls \
	mk6summary \
	mk6gather

dist_bin_SCRIPTS = \
	mk6cp

mk6gather_SOURCES = \
	mk6gather.c

mk6ls_SOURCES = \
	mk6ls.c

mk6summary_SOURCES = \
	mk6summary.c


