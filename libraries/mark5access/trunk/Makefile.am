SUBDIRS = \
	mark5access \
	bbsum \
	examples \
	doc

if ENABLE_PYTHON_CTYPES
SUBDIRS += python
endif

EXTRA_DIST = \
	TODO \
	mark5access.spec.in

DIST_SUBDIRS = $(SUBDIRS) python

dist-hook: mark5access.spec
	cp mark5access.spec $(distdir)

library_includedir = $(includedir)
library_include_HEADERS = mark5access.h

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = mark5access.pc

rpm: distdir
	tardir=$(distdir) && $(am__tar) | GZIP=$(GZIP_ENV) gzip -c >$(distdir).tar.gz
	rpmbuild -tb $(distdir).tar.gz
	$(am__remove_distdir)

