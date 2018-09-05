ACLOCAL_AMFLAGS=-I m4

SUBDIRS = jsmn mark6sg mark6gather test extra

EXTRA_DIST = COMPILING mark6sg.spec.in

dist-hook: mark6sg.spec
	cp mark6sg.spec $(distdir)

library_includedir = $(includedir)
library_include_HEADERS = mark6sg.h

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = mark6sg.pc

rpm: distdir
	tardir=$(distdir) && $(am__tar) | GZIP=$(GZIP_ENV) gzip -c >$(distdir).tar.gz
	rpmbuild -tb $(distdir).tar.gz
	$(am__remove_distdir)
