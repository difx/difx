SUBDIRS = \
	mark5access \
	bbsum \
	examples \
	doc

EXTRA_DIST = \
	TODO \
	mark5access.spec.in

dist-hook: mark5access.spec
	cp mark5access.spec $(distdir)

DIST_SUBDIRS = \
	mark5access \
	bbsum \
	examples \
	doc

library_includedir = $(includedir)
library_include_HEADERS = mark5access.h

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = mark5access.pc

rpm: distdir
	tardir=$(distdir) && $(am__tar) | GZIP=$(GZIP_ENV) gzip -c >$(distdir).tar.gz
	rpmbuild -tb $(distdir).tar.gz
	$(am__remove_distdir)

