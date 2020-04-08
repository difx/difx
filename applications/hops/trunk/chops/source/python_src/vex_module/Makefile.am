#
# $Id$
#

AM_CPPFLAGS = -I. -I$(srcdir)/include -I$(srcdir)/../../c_src/mk4util/include -I$(srcdir)/../../c_src/vex/include  -Wall -Wextra -DHAVE_CONFIG_H

pkginclude_HEADERS = ./include/vexpy_module_extern.h

pkglib_LTLIBRARIES = libvexpyb.la

libvexpyb_la_SOURCES = ./src/vexpy_module_extern.c

libvexpyb_la_LIBADD = ../../c_src/mk4util/libmk4utilb.la ../../c_src/vex/libvexb.la

libvexpyb_la_LDFLAGS = -avoid-version

if HAVE_PYTHON

SRC_FILES = vexpy/__init__.py \
	vexpy/vexpy.py

EXTRA_DIST = $(SRC_FILES) setup.py.in

all-local:
	(mkdir -p ./build)
	(rsync $(srcdir)/ -a .)
	(chmod -R u+rwx .)
	($(PYTHON) setup.py build --verbose)

install-exec-local:
	($(PYTHON) setup.py install \
		--prefix $(DESTDIR)$(prefix) \
		--record $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_vexpy_files.txt \
		--verbose)

uninstall-local:
	cat $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_vexpy_files.txt | xargs rm -rf
	rm -rf $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_vexpy_files.txt

CLEANFILES = $(SRC_FILES) ./include/vexpy_module_extern.h ./src/vexpy_module_extern.c setup.py.in

DISTCLEANFILES = setup.py

endif

#
# eof
#
