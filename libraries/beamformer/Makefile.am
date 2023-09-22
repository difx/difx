SUBDIRS = \
	src \
	examples

EXTRA_DIST = \
	beamformer.spec.in

dist-hook: beamformer.spec
	cp beamformer.spec $(distdir)

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = beamformer.pc

