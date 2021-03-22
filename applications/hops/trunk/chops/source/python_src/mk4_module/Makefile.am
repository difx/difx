#
# $Id$
#

AM_CPPFLAGS = -I. -I$(srcdir)/include -I$(srcdir)/../../c_src/mk4util/include -I$(srcdir)/../../c_src/afiob/include -Wall -Wextra -DHAVE_CONFIG_H

pkginclude_HEADERS = ./include/mk4_module_extern.h $(srcdir)/../../c_src/dfio/include/T1.h \
 $(srcdir)/../../c_src/dfio/include/bytflp.h $(srcdir)/../../c_src/dfio/include/mk4_data.h \
 $(srcdir)/../../c_src/dfio/include/mk4_dfio.h $(srcdir)/../../c_src/dfio/include/mk4_records.h \
 $(srcdir)/../../c_src/dfio/include/type_000.h $(srcdir)/../../c_src/dfio/include/type_100.h \
 $(srcdir)/../../c_src/dfio/include/type_101.h $(srcdir)/../../c_src/dfio/include/type_110.h \
 $(srcdir)/../../c_src/dfio/include/type_120.h $(srcdir)/../../c_src/dfio/include/type_200.h \
 $(srcdir)/../../c_src/dfio/include/type_201.h $(srcdir)/../../c_src/dfio/include/type_202.h \
 $(srcdir)/../../c_src/dfio/include/type_203.h $(srcdir)/../../c_src/dfio/include/type_204.h \
 $(srcdir)/../../c_src/dfio/include/type_205.h $(srcdir)/../../c_src/dfio/include/type_206.h \
 $(srcdir)/../../c_src/dfio/include/type_207.h $(srcdir)/../../c_src/dfio/include/type_208.h \
 $(srcdir)/../../c_src/dfio/include/type_210.h $(srcdir)/../../c_src/dfio/include/type_212.h \
 $(srcdir)/../../c_src/dfio/include/type_220.h $(srcdir)/../../c_src/dfio/include/type_221.h \
 $(srcdir)/../../c_src/dfio/include/type_222.h $(srcdir)/../../c_src/dfio/include/type_230.h \
 $(srcdir)/../../c_src/dfio/include/type_300.h $(srcdir)/../../c_src/dfio/include/type_301.h \
 $(srcdir)/../../c_src/dfio/include/type_302.h $(srcdir)/../../c_src/dfio/include/type_303.h \
 $(srcdir)/../../c_src/dfio/include/type_304.h $(srcdir)/../../c_src/dfio/include/type_305.h \
 $(srcdir)/../../c_src/dfio/include/type_306.h $(srcdir)/../../c_src/dfio/include/type_307.h \
 $(srcdir)/../../c_src/dfio/include/type_308.h $(srcdir)/../../c_src/dfio/include/type_309.h \
 $(srcdir)/../../c_src/dfio/include/type_comp.h

pkglib_LTLIBRARIES = libmk4iob.la

libmk4iob_la_SOURCES =  ./src/mk4_module_extern.c ./../../c_src/dfio/src/addr_100.c \
 ./../../c_src/dfio/src/addr_101.c ./../../c_src/dfio/src/addr_110.c \
 ./../../c_src/dfio/src/addr_120.c ./../../c_src/dfio/src/addr_200.c \
 ./../../c_src/dfio/src/addr_201.c ./../../c_src/dfio/src/addr_202.c \
 ./../../c_src/dfio/src/addr_203.c ./../../c_src/dfio/src/addr_204.c \
 ./../../c_src/dfio/src/addr_205.c ./../../c_src/dfio/src/addr_206.c \
 ./../../c_src/dfio/src/addr_207.c ./../../c_src/dfio/src/addr_208.c \
 ./../../c_src/dfio/src/addr_210.c ./../../c_src/dfio/src/addr_212.c \
 ./../../c_src/dfio/src/addr_220.c ./../../c_src/dfio/src/addr_221.c \
 ./../../c_src/dfio/src/addr_222.c ./../../c_src/dfio/src/addr_230.c \
 ./../../c_src/dfio/src/addr_300.c ./../../c_src/dfio/src/addr_301.c \
 ./../../c_src/dfio/src/addr_302.c ./../../c_src/dfio/src/addr_303.c \
 ./../../c_src/dfio/src/addr_304.c ./../../c_src/dfio/src/addr_305.c \
 ./../../c_src/dfio/src/addr_306.c ./../../c_src/dfio/src/addr_307.c \
 ./../../c_src/dfio/src/addr_308.c ./../../c_src/dfio/src/addr_309.c \
 ./../../c_src/dfio/src/clear_100.c ./../../c_src/dfio/src/clear_101.c \
 ./../../c_src/dfio/src/clear_110.c ./../../c_src/dfio/src/clear_120.c \
 ./../../c_src/dfio/src/clear_200.c ./../../c_src/dfio/src/clear_201.c \
 ./../../c_src/dfio/src/clear_202.c ./../../c_src/dfio/src/clear_203.c \
 ./../../c_src/dfio/src/clear_204.c ./../../c_src/dfio/src/clear_205.c \
 ./../../c_src/dfio/src/clear_206.c ./../../c_src/dfio/src/clear_207.c \
 ./../../c_src/dfio/src/clear_208.c ./../../c_src/dfio/src/clear_210.c \
 ./../../c_src/dfio/src/clear_212.c ./../../c_src/dfio/src/clear_220.c \
 ./../../c_src/dfio/src/clear_221.c ./../../c_src/dfio/src/clear_222.c \
 ./../../c_src/dfio/src/clear_230.c ./../../c_src/dfio/src/clear_300.c  \
 ./../../c_src/dfio/src/clear_301.c ./../../c_src/dfio/src/clear_302.c \
 ./../../c_src/dfio/src/clear_303.c ./../../c_src/dfio/src/clear_304.c \
 ./../../c_src/dfio/src/clear_305.c ./../../c_src/dfio/src/clear_306.c \
 ./../../c_src/dfio/src/clear_307.c ./../../c_src/dfio/src/clear_308.c \
 ./../../c_src/dfio/src/clear_309.c ./../../c_src/dfio/src/clear_coord.c \
 ./../../c_src/dfio/src/clear_mk4corel.c ./../../c_src/dfio/src/clear_mk4fringe.c \
 ./../../c_src/dfio/src/clear_mk4sdata.c ./../../c_src/dfio/src/compress.c \
 ./../../c_src/dfio/src/copy_100.c ./../../c_src/dfio/src/copy_101.c \
 ./../../c_src/dfio/src/copy_110.c ./../../c_src/dfio/src/copy_120.c \
 ./../../c_src/dfio/src/copy_200.c ./../../c_src/dfio/src/copy_201.c  \
 ./../../c_src/dfio/src/copy_202.c ./../../c_src/dfio/src/copy_203.c \
 ./../../c_src/dfio/src/copy_204.c ./../../c_src/dfio/src/copy_205.c \
 ./../../c_src/dfio/src/copy_206.c ./../../c_src/dfio/src/copy_207.c \
 ./../../c_src/dfio/src/copy_208.c ./../../c_src/dfio/src/copy_210.c \
 ./../../c_src/dfio/src/copy_212.c ./../../c_src/dfio/src/copy_220.c \
 ./../../c_src/dfio/src/copy_221.c ./../../c_src/dfio/src/copy_222.c \
 ./../../c_src/dfio/src/copy_230.c ./../../c_src/dfio/src/copy_300.c \
 ./../../c_src/dfio/src/copy_301.c ./../../c_src/dfio/src/copy_302.c \
 ./../../c_src/dfio/src/copy_303.c ./../../c_src/dfio/src/copy_304.c \
 ./../../c_src/dfio/src/copy_305.c ./../../c_src/dfio/src/copy_306.c \
 ./../../c_src/dfio/src/copy_307.c ./../../c_src/dfio/src/copy_308.c \
 ./../../c_src/dfio/src/copy_309.c ./../../c_src/dfio/src/corel_alloc.c \
 ./../../c_src/dfio/src/display_221.c ./../../c_src/dfio/src/init_000.c \
 ./../../c_src/dfio/src/open_datafile.c ./../../c_src/dfio/src/read_mk4corel.c \
 ./../../c_src/dfio/src/read_mk4file.c ./../../c_src/dfio/src/read_mk4fringe.c \
 ./../../c_src/dfio/src/read_mk4sdata.c ./../../c_src/dfio/src/test_compress.c \
 ./../../c_src/dfio/src/write_err.c ./../../c_src/dfio/src/write_mk4corel.c \
 ./../../c_src/dfio/src/write_mk4fringe.c ./../../c_src/dfio/src/write_mk4sdata.c \
 ./../../c_src/dfio/src/write_record.c

libmk4iob_la_LIBADD = ../../c_src/afio/libafiob.la ../../c_src/mk4util/libmk4utilb.la

libmk4iob_la_LDFLAGS = -avoid-version

SRC_FILES = mk4b/__init__.py \
	mk4b/mk4b.py \
	afiob/__init__.py \
	afiob/afiob.py

EXTRA_DIST = $(SRC_FILES) setup.py.in

if HAVE_PYTHON

all-local:
	(mkdir -p ./build)
	(rsync -av --exclude 'Makefile.*' $(srcdir)/ .)
	(chmod -R u+rwx .)
	($(PYTHON) setup.py build --verbose)

clean-local:
	rm -rf ./build

install-exec-local:
	($(PYTHON) setup.py install \
		--prefix $(DESTDIR)$(prefix) \
		--record $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_mk4_files.txt \
		--verbose)

uninstall-local:
	cat $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_mk4_files.txt | xargs rm -rf
	rm -rf $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_mk4_files.txt

CLEANFILES = $(SRC_FILES) ./include/mk4_module_extern.h ./src/mk4_module_extern.c setup.py.in

DISTCLEANFILES = setup.py

endif

#
# eof
#

