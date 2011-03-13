SUBDIRS = \
	src \
	utils

EXTRA_DIST = \
	vdifio.spec.in

dist-hook: vdifio.spec
	cp vdifio.spec $(distdir)

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = vdifio.pc

