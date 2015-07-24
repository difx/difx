EXTRA_DIST = \
	AUTHORS  COMPILING  LICENSE.txt  README  TODO \
	setup.py \
	Makefile.am \
	mark5access/__init__.py \
	mark5access/mark5access_helpers.py \
	mark5access/mark5access_writers.py \
	examples/m5selfcorr.py \
	examples/m5spec.py \
	examples/m5stat.py \
	examples/m5subband.py \
	examples/m5time.py \
	examples/m5tone.py \
	patch/codegenerator.patch

# Makefile.am to combine autotools and Python setuptools.
# Based on https://blog.kevin-brown.com/programming/2014/09/24/combining-autotools-and-setuptools.html

all-local:
	(cd $(srcdir); $(PYTHON) setup.py build \
	--build-base $(shell readlink -f $(builddir)))

install-exec-local:
	$(PYTHON) $(srcdir)/setup.py install \
	--prefix $(DESTDIR)$(prefix)
# not supported by very old Python setuptools:
#      --single-version-externally-managed \
#      --record $(DESTDIR)$(pkgpythondir)/install_files.txt \
#      --verbose
#
#uninstall-local:
#      cat $(DESTDIR)$(pkgpythondir)/install_files.txt | xargs rm -rf
#      rm -rf $(DESTDIR)$(pkgpythondir)

