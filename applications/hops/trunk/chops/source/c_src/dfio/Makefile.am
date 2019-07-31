#
# $Id$
#

SUBDIRS = .

AM_CPPFLAGS = -I. -I$(srcdir)/include -I$(srcdir)/../mk4util/include  -Wall -Wextra -DHAVE_CONFIG_H

pkginclude_HEADERS = ./include/T1.h ./include/bytflp.h ./include/mk4_data.h ./include/mk4_dfio.h ./include/mk4_records.h ./include/type_000.h ./include/type_100.h ./include/type_101.h ./include/type_110.h ./include/type_120.h ./include/type_200.h ./include/type_201.h ./include/type_202.h ./include/type_203.h ./include/type_204.h ./include/type_205.h ./include/type_206.h ./include/type_207.h ./include/type_208.h ./include/type_210.h ./include/type_212.h ./include/type_220.h ./include/type_221.h ./include/type_222.h ./include/type_230.h ./include/type_300.h ./include/type_301.h ./include/type_302.h ./include/type_303.h ./include/type_304.h ./include/type_305.h ./include/type_306.h ./include/type_307.h ./include/type_308.h ./include/type_309.h ./include/type_comp.h

pkglib_LTLIBRARIES = libdfiob.la

libdfiob_la_SOURCES = ./src/addr_100.c ./src/addr_101.c ./src/addr_110.c ./src/addr_120.c ./src/addr_200.c ./src/addr_201.c ./src/addr_202.c ./src/addr_203.c ./src/addr_204.c ./src/addr_205.c ./src/addr_206.c ./src/addr_207.c ./src/addr_208.c ./src/addr_210.c ./src/addr_212.c ./src/addr_220.c ./src/addr_221.c ./src/addr_222.c ./src/addr_230.c ./src/addr_300.c ./src/addr_301.c ./src/addr_302.c ./src/addr_303.c ./src/addr_304.c ./src/addr_305.c ./src/addr_306.c ./src/addr_307.c ./src/addr_308.c ./src/addr_309.c ./src/clear_100.c ./src/clear_101.c ./src/clear_110.c ./src/clear_120.c ./src/clear_200.c ./src/clear_201.c ./src/clear_202.c ./src/clear_203.c ./src/clear_204.c ./src/clear_205.c ./src/clear_206.c ./src/clear_207.c ./src/clear_208.c ./src/clear_210.c ./src/clear_212.c ./src/clear_220.c ./src/clear_221.c ./src/clear_222.c ./src/clear_230.c ./src/clear_300.c ./src/clear_301.c ./src/clear_302.c ./src/clear_303.c ./src/clear_304.c ./src/clear_305.c ./src/clear_306.c ./src/clear_307.c ./src/clear_308.c ./src/clear_309.c ./src/clear_coord.c ./src/clear_mk4corel.c ./src/clear_mk4fringe.c ./src/clear_mk4sdata.c ./src/compress.c ./src/copy_100.c ./src/copy_101.c ./src/copy_110.c ./src/copy_120.c ./src/copy_200.c ./src/copy_201.c ./src/copy_202.c ./src/copy_203.c ./src/copy_204.c ./src/copy_205.c ./src/copy_206.c ./src/copy_207.c ./src/copy_208.c ./src/copy_210.c ./src/copy_212.c ./src/copy_220.c ./src/copy_221.c ./src/copy_222.c ./src/copy_230.c ./src/copy_300.c ./src/copy_301.c ./src/copy_302.c ./src/copy_303.c ./src/copy_304.c ./src/copy_305.c ./src/copy_306.c ./src/copy_307.c ./src/copy_308.c ./src/copy_309.c ./src/corel_alloc.c ./src/display_221.c ./src/init_000.c ./src/open_datafile.c ./src/read_mk4corel.c ./src/read_mk4file.c ./src/read_mk4fringe.c ./src/read_mk4sdata.c ./src/test_compress.c ./src/write_err.c ./src/write_mk4corel.c ./src/write_mk4fringe.c ./src/write_mk4sdata.c ./src/write_record.c

libdfiob_la_LDFLAGS = -avoid-version

libdfiob_la_LIBADD = ../mk4util/libmk4utilb.la

check_SCRIPTS = ./import_dfio.sh
EXTRA_DIST = ./import_dfio.sh

#
# eof
#
