EXTRA_DIST = \
	mk5daemon.spec.in

dist-hook: mk5daemon.spec
	cp mk5daemon.spec $(distdir)

SUBDIRS = \
	src \
	utils \
	mk5dir

DIST_SUBDIRS = \
	src \
	utils \
	mk5dir

rpm: distdir
	tardir=$(distdir) && $(am__tar) | GZIP=$(GZIP_ENV) gzip -c >$(distdir).tar.gz
	rpmbuild -tb $(distdir).tar.gz
	$(am__remove_distdir)
