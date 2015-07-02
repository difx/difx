SUBDIRS = \
	src \
	utils

if ENABLE_PYTHON_CTYPES
SUBDIRS += python
endif

EXTRA_DIST = \
	vdifio.spec.in

DIST_SUBDIRS = $(SUBDIRS) python

dist-hook: vdifio.spec
	cp vdifio.spec $(distdir)

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = vdifio.pc

rpm: distdir
	tardir=$(distdir) && $(am__tar) | GZIP=$(GZIP_ENV) gzip -c >$(distdir).tar.gz
	rpmbuild -tb $(distdir).tar.gz
	$(am__remove_distdir)

