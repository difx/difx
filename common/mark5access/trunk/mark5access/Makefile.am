## Source directory

h_sources = mark5_stream.h
c_sources = \
	mark5_stream.c \
	mark5_stream_file.c \
	mark5_stream_memory.c \
	mark5_stream_unpacker.c \
	mark5_format_vlba.c \
	mark5_format_mark4.c \
	mark5_format_mark5b.c

library_includedir=$(includedir)/$(GENERIC_LIBRARY_NAME)-$(GENERIC_API_VERSION)/$(GENERIC_LIBRARY_NAME)
library_include_HEADERS = $(h_sources)

INCLUDES = -I$(top_srcdir)

lib_LTLIBRARIES= libmark5access-1.0.la
libmark5access_1_0_la_SOURCES= $(h_sources) $(c_sources)
libmark5access_1_0_la_LDFLAGS= -version-info $(GENERIC_LIBRARY_VERSION) -release $(GENERIC_RELEASE)

