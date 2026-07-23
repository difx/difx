#
# Makefile for vpal module
#

SRC_FILES = vpal/__init__.py vpal/report_lib.py vpal/phasecal.py 
CFG_FILES = vpal/ffres2pcp_lib.py \
	vpal/fourphase_lib.py \
	vpal/fringe_file_manipulation.py \
	vpal/pcc_delay_fitting.py \
	vpal/pcc_plotting_utils.py \
	vpal/processing.py \
	vpal/proxy_cable_cal.py \
	vpal/utility.py

EXTRA_DIST = $(SRC_FILES) setup.py.in

DISTCLEANFILES = setup.py $(SRC_FILES) $(CFG_FILES)

clean-local:
	rm -rf ./build ./dist vpal.egg-info
	[ -f Makefile.am ] || rm -rf vpal setup.py CMakeLists.txt

#if HAVE_PYTHON
if HAVE_PYTHON_EXTRA

$(CFG_FILES): $(top_builddir)/config.status
	cd $(top_builddir) && $(SHELL) ./config.status $(subdir)/$@

all-local: $(CFG_FILES) setup.py
	(mkdir -p ./build)
	(rsync -av --exclude 'Makefile.*' --exclude *.swp \
		  --exclude CMake* --exclude \*.in $(srcdir)/ .)
	(chmod -R u+w .)
	($(PYTHON) setup.py build --verbose)

#the awk gibberish is necessary to extract a list of files installed by pip
install-exec-local:
	[ -d $(DESTDIR)$(pythondir) ] || mkdir -p $(DESTDIR)$(pythondir)
	$(PYTHON) -m pip install . --upgrade --target=$(DESTDIR)$(pythondir)
	PYTHONUSERBASE=@prefix@ pip show -f vpal |\
	awk '/^Location:/ {sub(/^Location: /, "", $$0); loc=$$0} \
	     /^Files:/ {flag=1; next} \
		 flag && NF && gsub(" |\t","") {print loc "/" $$0}' |\
		 tee $(DESTDIR)$(pythondir)/install_vpal_files.txt

uninstall-local:
	cat $(DESTDIR)$(pythondir)/install_vpal_files.txt | xargs rm -rf
	rm -rf $(DESTDIR)$(pythondir)/install_vpal_files.txt
	rm -rf $(DESTDIR)$(pythondir)/vpal*
endif

#
# eof
#
