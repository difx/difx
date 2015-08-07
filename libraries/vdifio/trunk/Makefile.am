ACLOCAL_AMFLAGS = -I m4

if ENABLE_PYTHON_CTYPES
PYTHON_OPT = python
endif

SUBDIRS = \
	src \
	utils \
	$(PYTHON_OPT)

EXTRA_DIST = \
	vdifio.spec.in

dist-hook: vdifio.spec
	cp vdifio.spec $(distdir)

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = vdifio.pc

rpm: distdir
	tardir=$(distdir) && $(am__tar) | GZIP=$(GZIP_ENV) gzip -c >$(distdir).tar.gz
	rpmbuild -tb $(distdir).tar.gz
	$(am__remove_distdir)

