#
# $Id$
#

AM_CPPFLAGS = -I. -I$(srcdir)/include -I$(srcdir)/../../c_src/mk4util/include -I$(srcdir)/../../c_src/afiob/include -I$(srcdir)/../../c_src/dfio/include  -Wall -Wextra -DHAVE_CONFIG_H

pkginclude_HEADERS = ./include/mk4_module_extern.h

pkglib_LTLIBRARIES = libmk4iob.la

libmk4iob_la_SOURCES = ./src/mk4_module_extern.c

libmk4iob_la_LIBADD = ../../c_src/mk4util/libmk4utilb.la ../../c_src/afio/libafiob.la ../../c_src/dfio/libdfiob.la

libmk4iob_la_LDFLAGS = -avoid-version

if HAVE_PYTHON

SRC_FILES = mk4b/__init__.py \
	mk4b/mk4b.py \
	afiob/__init__.py \
	afiob/afiob.py

EXTRA_DIST = $(SRC_FILES) setup.py.in

all-local:
	(mkdir -p ./build)
	(rsync $(srcdir)/ -a .)
	(chmod -R u+rwx .)
	($(PYTHON) setup.py build --verbose)

install-exec-local:
	($(PYTHON) setup.py install \
		--prefix $(DESTDIR)$(prefix) \
		--record $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_mk4_files.txt \
		--verbose)
	(rm -r $(shell readlink -f $(builddir))/src)
	(rm -r $(shell readlink -f $(builddir))/build)
	(rm $(shell readlink -f $(builddir))/Makefile.am)
	(rm $(shell readlink -f $(builddir))/Makefile.in)

uninstall-local:
	cat $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_mk4_files.txt | xargs rm -rf
	rm -rf $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_mk4_files.txt

CLEANFILES = $(SRC_FILES) ./include/mk4_module_extern.h ./src/mk4_module_extern.c setup.py.in

DISTCLEANFILES = setup.py

endif 

#
# eof
#
