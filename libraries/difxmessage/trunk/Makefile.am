specfile = vlbi-difxmessage.spec

SUBDIRS = \
	difxmessage \
	mark5ipc \
	utils

EXTRA_DIST = \
	m4 \
	vlbi-difxmessage.spec.in

dist-hook: $(specfile)
	cp $(specfile) $(distdir)

library_includedir = $(includedir)
library_include_HEADERS = \
	difxmessage.h \
	mark5ipc.h

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = \
	difxmessage.pc \
	mark5ipc.pc

rpm: distdir
	tardir=$(distdir) && $(am__tar) | GZIP=$(GZIP_ENV) gzip -c >$(distdir).tar.gz
	rpmbuild -tb $(distdir).tar.gz
	$(am__remove_distdir)
