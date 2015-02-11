if WITH_EXPAT
expat_files = \
	difxparse.c
else
expat_files =
endif

h_sources = \
	difxmessageinternal.h

c_sources = \
	multicast.c \
	difxsend.c \
	difxreceive.c \
	difxmessageinit.c \
	difxsta.c \
	difxmessage.c

library_includedir = $(includedir)/difxmessage
library_include_HEADERS = $(h_sources)

Am_CPPFLAGS = -I$(top_srcdir)

lib_LTLIBRARIES = \
	libdifxmessage.la

libdifxmessage_la_SOURCES = $(h_sources) $(c_sources) $(expat_files)
libdifxmessage_la_LDFLAGS = -version-info $(LIBRARY_VERSION)

