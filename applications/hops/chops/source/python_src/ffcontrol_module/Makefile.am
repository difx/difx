#
# $Id$
#

AM_CPPFLAGS = -I. -I$(srcdir)/include -I$(srcdir)/../../c_src/mk4util/include -I$(srcdir)/../../c_src/fourfit_libs/ffcontrol/include  -Wall -Wextra -DHAVE_CONFIG_H

pkginclude_HEADERS = ./include/ffcontrol_module_extern.h

pkglib_LTLIBRARIES = libffcontrolpyb.la

libffcontrolpyb_la_SOURCES = ./src/construct_cblock.c ./src/ffcontrol_module_extern.c ./src/compute_control_file_hash.c

libffcontrolpyb_la_LIBADD = ../../c_src/mk4util/libmk4utilb.la ../../c_src/fourfit_libs/ffcontrol/libffcontrolb.la

libffcontrolpyb_la_LDFLAGS = -avoid-version

SRC_FILES = ffcontrol/__init__.py \
	ffcontrol/ffcontrol.py

EXTRA_DIST = $(SRC_FILES) setup.py.in

if HAVE_PYTHON

all-local:
	(mkdir -p ./build)
	(rsync -a --exclude 'Makefile.*' $(srcdir)/ .)
	(chmod -R u+rwx .)
	($(PYTHON) setup.py build --verbose)

clean-local:
	rm -rf ./build

install-exec-local:
	($(PYTHON) setup.py install \
		--prefix $(DESTDIR)$(prefix) \
		--record $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_ffcontrol_files.txt \
		--verbose)

uninstall-local:
	cat $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_ffcontrol_files.txt | xargs rm -rf
	rm -rf $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_ffcontrol_files.txt

CLEANFILES = $(SRC_FILES) ./include/ffcontrol_module_extern.h ./src/construct_cblock.c ./src/ffcontrol_module_extern.c ./src/compute_control_file_hash.c setup.py.in

DISTCLEANFILES = setup.py

endif

#
# eof
#
