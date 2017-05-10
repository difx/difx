#
# $Id$
#

PKG_NAME=phorc

SRC_FILES = src/phorc/__init__.py \
	src/phorc/baseline_fringe_product_list.py \
	src/phorc/ffres2pcp_lib.py \
	src/phorc/fourphase_lib.py \
	src/phorc/fringe_file_handle.py \
	src/phorc/plotting.py \
	src/phorc/processing.py \
	src/phorc/scripting.py

EXTRA_DIST = $(SRC_FILES) setup.py

all-local:
	(mkdir -p ./build)
	(rsync $(srcdir)/ -a .)
	(chmod -R u+rwx .)
	($(PYTHON) setup.py build --verbose)

install-exec-local:
	($(PYTHON) setup.py install \
		--prefix $(DESTDIR)$(prefix) \
		--single-version-externally-managed \
		--record $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_files.txt \
		--verbose)
	(rm -r $(shell readlink -f $(builddir))/src)
	(rm -r $(shell readlink -f $(builddir))/build)
	(rm $(shell readlink -f $(builddir))/Makefile.am)
	(rm $(shell readlink -f $(builddir))/Makefile.in)
	(rm $(shell readlink -f $(builddir))/setup.py)

uninstall-local:
	cat $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_files.txt | xargs rm -rf
	rm -rf $(DESTDIR)$(pythondir)/$(PKG_NAME)
	
	

# all-local:
# 	(rsync $(srcdir)/ -a .) 
# 	(cd $(srcdir); $(PYTHON) setup.py egg_info \
# 		--egg-base $(shell readlink -f $(builddir))/build \
# 		--verbose)
# 	(cd $(srcdir); $(PYTHON) setup.py build \
# 		--build-base $(shell readlink -f $(builddir))/build \
# 		--verbose)
# 
# install-exec-local:
# 	(cd $(srcdir); $(PYTHON) setup.py egg_info \
# 		--egg-base $(shell readlink -f $(builddir))/build \
# 		--verbose)
# 	$(PYTHON) $(srcdir)/setup.py install \
# 		--prefix $(DESTDIR)$(prefix) \
# 		--single-version-externally-managed \
# 		--record $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_files.txt \
# 		--verbose
# 
# uninstall-local:
# 	cat $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_files.txt | xargs rm -rf
# 	rm -rf $(DESTDIR)$(pythondir)/$(PKG_NAME)


#
# eof
#
