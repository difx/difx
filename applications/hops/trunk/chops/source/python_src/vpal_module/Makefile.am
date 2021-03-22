#
# $Id$
#

SRC_FILES = vpal/__init__.py \
	vpal/utility.py \
	vpal/ffres2pcp_lib.py \
	vpal/fourphase_lib.py \
	vpal/report_lib.py \
	vpal/proxy_cable_cal.py \
	vpal/pcc_delay_fitting.py \
	vpal/pcc_plotting_utils.py \
	vpal/fringe_file_manipulation.py \
	vpal/phasecal.py \
	vpal/processing.py

EXTRA_DIST = $(SRC_FILES) setup.py.in

if HAVE_PYTHON

all-local:
	(mkdir -p ./build)
	(rsync -av --exclude 'Makefile.*' --exclude *.swp \
		  --exclude CMake* --exclude deprecated_modules \
		  $(srcdir)/ .)
	(chmod -R u+rwx .)
	($(PYTHON) setup.py build --verbose)

clean-local:
	rm -rf ./build vpal deprecated_modules

install-exec-local:
	($(PYTHON) setup.py install \
		--prefix $(DESTDIR)$(prefix) \
		--record $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_vpal_files.txt \
		--verbose)

uninstall-local:
	cat $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_vpal_files.txt | xargs rm -rf
	rm -rf $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_vpal_files.txt

CLEANFILES = $(SRC_FILES) setup.py.in

DISTCLEANFILES = setup.py

endif

#
# eof
#
