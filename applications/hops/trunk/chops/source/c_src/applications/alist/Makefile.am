#
# $Id$
#

SUBDIRS = .

AM_CPPFLAGS = -I. -I$(srcdir)/include -I$(srcdir)/../../mk4util/include -I$(srcdir)/../../afio/include -I$(srcdir)/../../dfio/include  -Wall -Wextra -DHAVE_CONFIG_H

bin_PROGRAMS = balist

balist_SOURCES = alist.c open_output.c parse_cmdline.c summarize_mk4fringe.c

balist_LDADD = ../../mk4util/libmk4utilb.la ../../afio/libafiob.la ../../dfio/libdfiob.la

#added manually (should this be configured?)
balist_LDADD += -lm

check_SCRIPTS = ./import_alist.sh
EXTRA_DIST = ./import_alist.sh


#
# eof
#
