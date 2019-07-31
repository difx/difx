#
# $Id$
#

SUBDIRS = .

AM_CPPFLAGS = -I. -I$(srcdir)/include -I$(srcdir)/../../mk4util/include -I$(srcdir)/../../afio/include -I$(srcdir)/../../dfio/include -I$(srcdir)/../../vex/include -I$(srcdir)/../../fourfit_libs/ffcontrol/include -I$(srcdir)/../../fourfit_libs/ffmath/include -I$(srcdir)/../../fourfit_libs/ffcore/include -I$(srcdir)/../../fourfit_libs/ffio/include -I$(srcdir)/../../fourfit_libs/ffsearch/include -I$(srcdir)/../../fourfit_libs/ffplot/include  -Wall -Wextra -DHAVE_CONFIG_H -DFF_PROGNAME=\"fourfit\" -DFF_VER_NO=\"0.00\"

AM_CPPFLAGS += ${FFTW3_CFLAGS} ${X_CFLAGS} -I${PGPLOT_DIR}

bin_PROGRAMS = bfourfit

bfourfit_SOURCES = fourfit.c

bfourfit_LDADD = ../../mk4util/libmk4utilb.la ../../afio/libafiob.la ../../dfio/libdfiob.la ../../vex/libvexb.la ../../fourfit_libs/ffcontrol/libffcontrolb.la ../../fourfit_libs/ffmath/libffmathb.la ../../fourfit_libs/ffcore/libffcoreb.la ../../fourfit_libs/ffio/libffiob.la ../../fourfit_libs/ffsearch/libffsearchb.la ../../fourfit_libs/ffplot/libffplotb.la

bfourfit_LDADD += ${PGP_LIB} ${PNG_LIB} ${X_FPLOT_LIB} ${X11_LIB} ${FFTW3_LIBS}

check_SCRIPTS = ./import_fourfit.sh
EXTRA_DIST = ./import_fourfit.sh

#
# eof
#
