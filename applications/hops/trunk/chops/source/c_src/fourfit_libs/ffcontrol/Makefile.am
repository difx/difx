#
# $Id$
#

SUBDIRS = .

AM_CPPFLAGS = -I. -I$(srcdir)/include -I$(srcdir)/../../mk4util/include -DHAVE_CONFIG_H -I$(srcdir)/../ffcore/include

pkginclude_HEADERS = ./include/control.h ./include/parser.h

pkglib_LTLIBRARIES = libffcontrolb.la

libffcontrolb_la_SOURCES = ./src/copy_cblock_parts.c ./src/create_fsm.c ./src/criteria_match.c ./src/default_cblock.c ./src/get_bfstring.c ./src/init_tokens.c ./src/lex.c ./src/nullify_cblock.c ./src/parse_control_file.c ./src/parser.c ./src/read_control_file.c ./src/skip_data.c ./src/skip_index.c

libffcontrolb_la_LIBADD = ../../mk4util/libmk4utilb.la

libffcontrolb_la_LDFLAGS = -avoid-version

check_SCRIPTS = ./import_ffcontrol.sh
EXTRA_DIST = ./import_ffcontrol.sh

#
# eof
#
