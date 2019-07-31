#
# $Id$
#

SUBDIRS = .

if HAVE_FFTW

AM_CPPFLAGS = -I. -I$(srcdir)/include -I$(srcdir)/../../mk4util/include -I$(srcdir)/../../afio/include -I$(srcdir)/../../dfio/include -I$(srcdir)/../../vex/include -I$(srcdir)/../ffcontrol/include -I$(srcdir)/../ffcore/include -I$(srcdir)/../ffio/include -I$(srcdir)/../ffmath/include  -Wall -Wextra -DHAVE_CONFIG_H

pkginclude_HEADERS = ./include/adhoc_flag.h

pkglib_LTLIBRARIES = libffsearchb.la

libffsearchb_la_SOURCES = ./src/adhoc_flag.c ./src/apply_filter.c ./src/apply_notches.c ./src/apply_passband.c ./src/calc_normalization.c ./src/calc_rms.c ./src/compute_model.c ./src/compute_qf.c ./src/create_fname.c ./src/delay_rate.c ./src/est_pc_manual.c ./src/freq_spacing.c ./src/fringe_search.c ./src/gate_off.c ./src/interp.c ./src/ion_search.c ./src/norm_fx.c ./src/norm_xf.c ./src/organize_data.c ./src/parse_cmdline.c ./src/pcalibrate.c ./src/precorrect.c ./src/report_actions.c ./src/rotate_pcal.c ./src/sampler_delays.c ./src/search.c ./src/search_windows.c ./src/update.c ./src/apply_video_bp.c 

libffsearchb_la_LIBADD = ../../mk4util/libmk4utilb.la ../../afio/libafiob.la ../../dfio/libdfiob.la ../../vex/libvexb.la ../ffcontrol/libffcontrolb.la ../ffcore/libffcoreb.la ../ffio/libffiob.la ../ffmath/libffmathb.la

libffsearchb_la_LIBADD += ${FFTW3_LIBS}

libffsearchb_la_LDFLAGS = -avoid-version

check_SCRIPTS = ./import_ffsearch.sh
EXTRA_DIST = ./import_ffsearch.sh

else !HAVE_FFTW

libffsearchb_la_SOURCES = you_lose.c

endif !HAVE_FFTW

you_lose.c: Makefile.am
	echo 'main(){return(' > $@
	echo 'puts("One or more of the required packages:")+' >> $@
	echo 'puts("PNG, X11, PGPLOT or FFTW3 is missing.")+' >> $@
	echo 'puts("Check your installation are recompile.")' >> $@
	echo ');}' >> $@

CLEANFILES = you_lose.c

#
# eof
#
