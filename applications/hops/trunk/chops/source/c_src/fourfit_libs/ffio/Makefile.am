#
# $Id$
#

SUBDIRS = .

AM_CPPFLAGS = -I. -I$(srcdir)/include -I$(srcdir)/../../mk4util/include -I$(srcdir)/../../afio/include -I$(srcdir)/../../dfio/include -I$(srcdir)/../../vex/include -I$(srcdir)/../ffcontrol/include -I$(srcdir)/../ffcore/include -DHAVE_CONFIG_H

pkginclude_HEADERS =

pkglib_LTLIBRARIES = libffiob.la

libffiob_la_LDFLAGS = -avoid-version

libffiob_la_SOURCES = ./src/fill_200.c ./src/fill_201.c ./src/fill_202.c ./src/fill_203.c ./src/fill_204.c ./src/fill_205.c ./src/fill_206.c ./src/fill_207.c ./src/fill_208.c ./src/fill_210.c ./src/fill_212.c ./src/fill_222.c ./src/fill_230.c ./src/fill_fringe_info.c

libffiob_la_LIBADD = ../../mk4util/libmk4utilb.la ../../afio/libafiob.la ../../dfio/libdfiob.la ../../vex/libvexb.la ../ffcontrol/libffcontrolb.la ../ffcore/libffcoreb.la

check_SCRIPTS = ./import_ffio.sh
EXTRA_DIST = ./import_ffio.sh


#
# eof
#
