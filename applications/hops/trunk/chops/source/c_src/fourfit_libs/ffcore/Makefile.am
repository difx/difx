#
# $Id$
#

SUBDIRS = .

AM_CPPFLAGS = -I. -I$(srcdir)/include -I$(srcdir)/../../mk4util/include -I$(srcdir)/../../afio/include -I$(srcdir)/../../dfio/include -I$(srcdir)/../../vex/include -I$(srcdir)/../ffcontrol/include -DHAVE_CONFIG_H

pkginclude_HEADERS = ./include/filter.h ./include/fourfit_signal_handler.h ./include/freqlist.h ./include/param_struct.h ./include/pass_struct.h ./include/plot_struct.h ./include/refringe.h ./include/statistics.h ./include/write_lock_mechanism.h ./include/ff_misc_if.h

pkglib_LTLIBRARIES = libffcoreb.la

libffcoreb_la_SOURCES = ./src/check_rflist.c ./src/clear_freq_corel.c ./src/clear_pass.c ./src/create_lockfile.c ./src/diff_file_phase.c ./src/fill_param.c ./src/fourfit_signal_handler.c ./src/generate_cblock.c ./src/get_corel_data.c ./src/make_flist.c ./src/make_passes.c ./src/pcal_interp.c ./src/read_sdata.c ./src/refringe_list.c ./src/set_defaults.c ./src/set_pointers.c ./src/stcount_interp.c ./src/time_range.c ./src/update.c ./src/vrot.c ./src/wait_for_write_lock.c

libffcoreb_la_LIBADD = ../../mk4util/libmk4utilb.la ../../afio/libafiob.la ../../dfio/libdfiob.la ../../vex/libvexb.la ../ffcontrol/libffcontrolb.la

libffcoreb_la_LDFLAGS = -avoid-version

TESTS = ff_misc_if

check_SCRIPTS = ./import_ffcore.sh
check_PROGRAMS = ff_misc_if

ff_misc_if_SOURCES = ./src/ff_misc_if.c

EXTRA_DIST = ./import_ffcore.sh

#
# eof
#
