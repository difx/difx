
EXTRA_DIST = setup.py Makefile.am README examples/vdifio_1.py examples/vdifio_1.vdif vdifio/__init__.py

# Makefile.am to combine autotools and Python setuptools.
# Based on https://blog.kevin-brown.com/programming/2014/09/24/combining-autotools-and-setuptools.html

all-local:
	(cd $(srcdir); $(PYTHON) setup.py build \
	--build-base $(shell readlink -f $(builddir)))

install-exec-local:
	mkdir -p $(DESTDIR)$(prefix)/lib/python/
	$(PYTHON) $(srcdir)/setup.py install \
	--prefix $(DESTDIR)$(prefix) --install-lib=$(DESTDIR)$(prefix)/lib/python/
# TODO: DiFX uses peculiar location, not the standard site-packages, need to choose lib vs lib64 above

# not supported by very old Python setuptools:
#	--single-version-externally-managed \
#	--record $(DESTDIR)$(pkgpythondir)/install_files.txt \
#	--verbose
#
#uninstall-local:
#	cat $(DESTDIR)$(pkgpythondir)/install_files.txt | xargs rm -rf
#	rm -rf $(DESTDIR)$(pkgpythondir)
