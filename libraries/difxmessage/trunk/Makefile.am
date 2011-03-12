SUBDIRS = \
	difxmessage \
	mark5ipc \
	utils

EXTRA_DIST = \
	difxmessage.spec.in

dist-hook: difxmessage.spec
	cp difxmessage.spec $(distdir)

library_includedir = $(includedir)
library_include_HEADERS = \
	difxmessage.h \
	mark5ipc.h

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = \
	difxmessage.pc \
	mark5ipc.pc

