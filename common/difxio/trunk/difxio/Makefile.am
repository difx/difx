## Source directory

h_sources = difx_input.h  parsedifx.h  parsevis.h
c_sources = difx_input.c  parsedifx.c  parsevis.c

library_includedir=$(includedir)/$(GENERIC_LIBRARY_NAME)-$(GENERIC_API_VERSION)/$(GENERIC_LIBRARY_NAME)
library_include_HEADERS = $(h_sources)

INCLUDES = -I$(top_srcdir)

lib_LTLIBRARIES= libdifxio-1.0.la
libdifxio_1_0_la_SOURCES= $(h_sources) $(c_sources)
libdifxio_1_0_la_LDFLAGS= -version-info $(GENERIC_LIBRARY_VERSION) -release $(GENERIC_RELEASE)

