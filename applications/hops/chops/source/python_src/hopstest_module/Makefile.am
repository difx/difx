#
# Makefile for hopstest module
#

# configure generates CFG_FILES and automatically adds them to dist
SRC_FILES = hopstestb/__init__.py
CFG_FILES = hopstestb/hopstestb.py

EXTRA_DIST = $(SRC_FILES) setup.py.in

DISTCLEANFILES = setup.py $(SRC_FILES) $(CFG_FILES)

clean-local:
	rm -rf ./build ./dist hopstestb.egg-info CMakeLists.txt
	[ -f Makefile.am ] || rm -rf hopstest* setup.py CMakeLists.txt
	
#if HAVE_PYTHON
if HAVE_PYTHON_EXTRA

dist_bin_SCRIPTS =  hopstestsuiteb.py

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
	loc=$(pip show -f hopstestb | sed -n 's/^Location: //p')
	PYTHONUSERBASE=@prefix@ pip show -f hopstestb |\
	awk '/^Location:/ {sub(/^Location: /, "", $$0); loc=$$0} \
	     /^Files:/ {flag=1; next} \
		 flag && NF && gsub(" |\t","") {print loc "/" $$0}' |\
		 tee $(DESTDIR)$(pythondir)/install_hopstestb_files.txt

uninstall-local:
	cat $(DESTDIR)$(pythondir)/install_hopstestb_files.txt | xargs rm -rf
	rm -rf $(DESTDIR)$(pythondir)/install_hopstestb_files.txt
	rm -rf $(DESTDIR)$(pythondir)/hopstestb*

endif

#
# eof
#
