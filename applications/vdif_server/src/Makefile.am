h_sources = vdif_server.h

library_includedir = $(includedir)/vdif_server
library_include_headers = vdif_server.h

lib_LIBRARIES = libvdif_server.a
libvdif_server_a_SOURCES = vdif_server.cpp
include_HEADERS = vdif_server.h

bin_PROGRAMS = vdif_server

AM_CXXFLAGS = $(DIFXMESSAGE_CFLAGS) $(FXCORR_CFLAGS) $(IPP_CFLAGS) $(DIFXIO_CFLAGS) $(VDIFIO_CFLAGS)

vdif_server_SOURCES = vdif_server.cpp
vdif_server_LDADD = $(DIFXMESSAGE_LIBS) $(FXCORR_LIBS) $(IPP_LIBS) $(DIFXIO_LIBS) $(VDIFIO_LIBS)
vdif_server_LDFLAGS = -pthread
