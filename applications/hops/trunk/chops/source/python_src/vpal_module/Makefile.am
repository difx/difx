#
# $Id$
#

if HAVE_PYTHON

SRC_FILES = vpal/__init__.py \
	vpal/utility.py \
	vpal/ffres2pcp_lib.py \
	vpal/fourphase_lib.py \
	vpal/report_lib.py \
	vpal/proxy_cable_cal.py \
	vpal/pcc_delay_fitting.py \
	vpal/pcc_plotting_utils.py \
	vpal/fringe_file_manipulation.py \
	vpal/processing.py


EXTRA_DIST = $(SRC_FILES) setup.py.in

all-local:
	(mkdir -p ./build)
	(rsync $(srcdir)/ -a .)
	(chmod -R u+rwx .)
	($(PYTHON) setup.py build --verbose)

install-exec-local:
	($(PYTHON) setup.py install \
		--prefix $(DESTDIR)$(prefix) \
		--record $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_vpal_files.txt \
		--verbose)
	(rm -r $(shell readlink -f $(builddir))/build)
	(rm $(shell readlink -f $(builddir))/Makefile.am)
	(rm $(shell readlink -f $(builddir))/Makefile.in)

uninstall-local:
	cat $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_vpal_files.txt | xargs rm -rf
	rm -rf $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_vpal_files.txt

CLEANFILES = $(SRC_FILES) setup.py.in

DISTCLEANFILES = setup.py

endif

#
# eof
#
