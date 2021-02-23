
if USE_MARK6SG
mark6sg_h_sources = mark6gather_mark5b.h
mark6sg_c_sources = mark6gather_mark5b.c
else
mark6sg_h_sources =
mark6sg_c_sources =
endif

if USE_CODIFIO
codif_c_sources = format_codif.c
else
codif_c_sources = format_codif_stubs.c
endif

h_sources = \
        $(mark6sg_h_sources) \
	mark5_stream.h \
	mark5bfix.h \
	mark5bfile.h

c_sources = \
        $(mark6sg_c_sources) \
	mark5_stream.c \
	mark5_stream_file.c \
	mark5_stream_memory.c \
	mark5_stream_unpacker.c \
	mark5_format_vlba.c \
	mark5_format_vlba_nomod.c \
	mark5_format_mark4.c \
	mark5_format_mark5b.c \
	mark5_format_kvn5b.c \
	mark5_format_d2k.c \
	format_vdif.c \
	$(codif_c_sources) \
	blanker_none.c \
	blanker_mark5.c \
	mark5bfix.c \
	mark5bfile.c

library_includedir = $(includedir)/mark5access
library_include_HEADERS = $(h_sources)

AM_CPPFLAGS = -I$(top_srcdir) $(CODIFIO_CFLAGS)

lib_LTLIBRARIES = \
	libmark5access.la

libmark5access_la_SOURCES = $(h_sources) $(c_sources)
libmark5access_la_LDFLAGS = -version-info $(LIBRARY_VERSION)

