#
# $Id$
#

if HAVE_PYTHON

SRC_FILES = hopstestb/__init__.py \
	hopstestb/hopstestb.py

EXTRA_DIST = $(SRC_FILES) setup.py.in

dist_bin_SCRIPTS =  hopstestsuiteb.py

all-local:
	(mkdir -p ./build)
	(rsync $(srcdir)/ -a .)
	(chmod -R u+rwx .)
	($(PYTHON) setup.py build --verbose)

install-exec-local:
	($(PYTHON) setup.py install \
		--prefix $(DESTDIR)$(prefix) \
		--record $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_hopstest_files.txt \
		--verbose)
	(rm -r $(shell readlink -f $(builddir))/build)
	(rm $(shell readlink -f $(builddir))/Makefile.am)
	(rm $(shell readlink -f $(builddir))/Makefile.in)

uninstall-local:
	cat $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_hopstest_files.txt | xargs rm -rf
	rm -rf $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_hopstest_files.txt

CLEANFILES = $(SRC_FILES) setup.py.in hopstestsuiteb.py.in 

DISTCLEANFILES = setup.py

endif 

#
# eof
#
