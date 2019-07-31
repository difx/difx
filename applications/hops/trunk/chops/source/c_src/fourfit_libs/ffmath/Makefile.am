#
# $Id$
#

SUBDIRS = .

AM_CPPFLAGS = -I. -I$(srcdir)/include -I$(srcdir)/../../mk4util/include -I$(srcdir)/../../dfio/include  -Wall -Wextra -DHAVE_CONFIG_H

pkginclude_HEADERS = 

pkglib_LTLIBRARIES = libffmathb.la

libffmathb_la_SOURCES = ./src/ap_mean.c ./src/bcd_to_2int.c ./src/interp555.c ./src/linterp.c ./src/max555.c ./src/parabola.c

libffmathb_la_LIBADD = ../../mk4util/libmk4utilb.la ../../dfio/libdfiob.la

libffmathb_la_LDFLAGS = -avoid-version

check_SCRIPTS = ./import_ffmath.sh
EXTRA_DIST = ./import_ffmath.sh

#
# eof
#
