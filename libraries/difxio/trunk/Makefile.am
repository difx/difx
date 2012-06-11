SUBDIRS = \
	difxio \
	utils \
	tests

EXTRA_DIST = \
	difxio.spec.in

dist-hook: difxio.spec
	cp difxio.spec $(distdir)

library_includedir = $(includedir)
library_include_HEADERS = difxio.h

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = difxio.pc

rpm: distdir
	tardir=$(distdir) && $(am__tar) | GZIP=$(GZIP_ENV) gzip -c >$(distdir).tar.gz
	rpmbuild -tb $(distdir).tar.gz
	$(am__remove_distdir)
