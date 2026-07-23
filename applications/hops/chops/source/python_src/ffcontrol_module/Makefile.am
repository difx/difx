#
# Makefile for ffcontrol module
#

AM_CPPFLAGS = -I. -I$(srcdir)/include \
	-I$(srcdir)/../../c_src/mk4util/include \
	-I$(srcdir)/../../c_src/fourfit_libs/ffcontrol/include \
	-Wall -Wextra -DHAVE_CONFIG_H

pkginclude_HEADERS = include/ffcontrol_module_extern.h
pkglib_LTLIBRARIES = libffcontrolpyb.la

# note that these are rsync'd to build area due to setup.py
ffcontrolsources = src/construct_cblock.c \
	src/ffcontrol_module_extern.c \
	src/compute_control_file_hash.c

# and for some reason, some older automakes have trouble with
# library dependencies
BUILT_SOURCES = $(ffcontrolsources)
$(ffcontrolsources): importstamp
importstamp: $(ffcontrolsources) $(pkginclude_HEADERS)
	[ -f Makefile.am ] || {\
		[ -d src ] || mkdir src ;\
		for s in $(ffcontrolsources);\
			do [ -f $$s ] || cp -p $(srcdir)/$$s src ; done ;\
		[ -d include ] || mkdir include ;\
		for s in $(pkginclude_HEADERS);\
			do [ -f $$s ] || cp -p $(srcdir)/$$s src ; done ;}
	touch importstamp

nodist_libffcontrolpyb_la_SOURCES = $(ffcontrolsources)

libffcontrolpyb_la_LIBADD = ../../c_src/mk4util/libmk4utilb.la \
	../../c_src/fourfit_libs/ffcontrol/libffcontrolb.la

libffcontrolpyb_la_LDFLAGS = -avoid-version

# configure generates CFG_FILES and automatically adds them to dist
SRC_FILES = ffcontrol/__init__.py $(ffcontrolsources)
CFG_FILES = ffcontrol/ffcontrol.py

EXTRA_DIST = $(SRC_FILES) setup.py.in

DISTCLEANFILES = setup.py $(SRC_FILES) $(CFG_FILES)

CLEANFILES = importstamp

clean-local:
	rm -rf ./build ./dist ffcontrol.egg-info
	[ -f Makefile.am ] || rm -rf ffcontrol setup.py include src CMakeLists.txt

#if HAVE_PYTHON
if HAVE_PYTHON_EXTRA
# note the above library is useless without python

$(CFG_FILES): $(top_builddir)/config.status
	cd $(top_builddir) && $(SHELL) ./config.status $(subdir)/$@

all-local: $(CFG_FILES) setup.py
	(mkdir -p ./build)
	(rsync -a --exclude 'Makefile.*' --exclude *.swp \
		--exclude CMake* --exclude \*.in $(srcdir)/ .)
	(chmod -R u+w .)
	($(PYTHON) setup.py build --verbose)

#the awk gibberish is necessary to extract a list of files installed by pip
install-exec-local:
	[ -d $(DESTDIR)$(pythondir) ] || mkdir -p $(DESTDIR)$(pythondir)
	$(PYTHON) -m pip install . --upgrade --target=$(DESTDIR)$(pythondir)
	PYTHONUSERBASE=@prefix@ pip show -f ffcontrol |\
	awk '/^Location:/ {sub(/^Location: /, "", $$0); loc=$$0} \
         /^Files:/ {flag=1; next} \
         flag && NF && gsub(" |\t","") {print loc "/" $$0}' |\
         tee $(DESTDIR)$(pythondir)/install_ffcontrol_files.txt

uninstall-local:
	cat $(DESTDIR)$(pythondir)/install_ffcontrol_files.txt | xargs rm -rf
	rm -rf $(DESTDIR)$(pythondir)/install_ffcontrol_files.txt
	rm -rf $(DESTDIR)$(pythondir)/ffcontrol*
endif

#
# eof
#
