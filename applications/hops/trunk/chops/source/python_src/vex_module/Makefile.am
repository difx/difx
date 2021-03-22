#
# $Id$
#

AM_CPPFLAGS = -I. -I$(srcdir)/include -I$(srcdir)/../../c_src/mk4util/include -I$(srcdir)/../../c_src/vex/include  -Wall -Wextra -DHAVE_CONFIG_H

pkginclude_HEADERS = ./include/vexpy_module_extern.h $(srcdir)/../../c_src/vex/include/cvex.h \
 $(srcdir)/../../c_src/vex/include/evex.h $(srcdir)/../../c_src/vex/include/ivex.h \
 $(srcdir)/../../c_src/vex/include/lvex.h $(srcdir)/../../c_src/vex/include/mk4_vex.h \
 $(srcdir)/../../c_src/vex/include/ovex.h $(srcdir)/../../c_src/vex/include/svex.h \
 $(srcdir)/../../c_src/vex/include/vex.h

pkglib_LTLIBRARIES = libvexpyb.la

libvexpyb_la_SOURCES = ./src/vexpy_module_extern.c ./../../c_src/vex/src/block_params.c \
 ./../../c_src/vex/src/check_intrange.c ./../../c_src/vex/src/check_realrange.c \
 ./../../c_src/vex/src/check_scan.c ./../../c_src/vex/src/check_stloc.c \
 ./../../c_src/vex/src/check_strrange.c ./../../c_src/vex/src/cvex_info.c \
 ./../../c_src/vex/src/decode_pval.c ./../../c_src/vex/src/do_antenna.c \
 ./../../c_src/vex/src/do_bbc.c ./../../c_src/vex/src/do_clock.c \
 ./../../c_src/vex/src/do_das.c ./../../c_src/vex/src/do_eop.c \
 ./../../c_src/vex/src/do_exper.c ./../../c_src/vex/src/do_freq.c \
 ./../../c_src/vex/src/do_head_pos.c ./../../c_src/vex/src/do_if.c \
 ./../../c_src/vex/src/do_pass_order.c ./../../c_src/vex/src/do_phase_cal_detect.c \
 ./../../c_src/vex/src/do_roll.c ./../../c_src/vex/src/do_site.c \
 ./../../c_src/vex/src/do_source.c ./../../c_src/vex/src/do_track.c \
 ./../../c_src/vex/src/evex_info.c ./../../c_src/vex/src/fill_deflists.c \
 ./../../c_src/vex/src/fill_scanlist.c ./../../c_src/vex/src/fill_station_parms.c \
 ./../../c_src/vex/src/find_statements.c ./../../c_src/vex/src/get_block_mode.c \
 ./../../c_src/vex/src/get_chip_mode.c ./../../c_src/vex/src/get_corr_bd_parms.c \
 ./../../c_src/vex/src/get_corr_mode.c ./../../c_src/vex/src/get_def.c \
 ./../../c_src/vex/src/get_drive_init.c ./../../c_src/vex/src/get_global_deflist.c \
 ./../../c_src/vex/src/get_logscan.c ./../../c_src/vex/src/get_mode_deflist.c \
 ./../../c_src/vex/src/get_pbs_init.c ./../../c_src/vex/src/get_pcm_config.c \
 ./../../c_src/vex/src/get_pcm_tables.c ./../../c_src/vex/src/get_section_mode.c \
 ./../../c_src/vex/src/get_statement.c ./../../c_src/vex/src/get_station_deflist.c \
 ./../../c_src/vex/src/get_su_chan_out.c ./../../c_src/vex/src/get_su_connect.c \
 ./../../c_src/vex/src/get_trm_config.c ./../../c_src/vex/src/get_val_list.c \
 ./../../c_src/vex/src/get_version.c ./../../c_src/vex/src/get_vex.c \
 ./../../c_src/vex/src/in_comment.c ./../../c_src/vex/src/in_quote.c \
 ./../../c_src/vex/src/init_scan.c ./../../c_src/vex/src/ivex_info.c \
 ./../../c_src/vex/src/locate_blocks.c ./../../c_src/vex/src/locate_cq.c \
 ./../../c_src/vex/src/lvex_info.c ./../../c_src/vex/src/nextchar.c \
 ./../../c_src/vex/src/param_formats.c ./../../c_src/vex/src/parse_date.c \
 ./../../c_src/vex/src/parse_dec.c ./../../c_src/vex/src/parse_pval.c \
 ./../../c_src/vex/src/parse_ra.c ./../../c_src/vex/src/parse_ref.c \
 ./../../c_src/vex/src/parse_units.c ./../../c_src/vex/src/parse_vexfile.c \
 ./../../c_src/vex/src/print_location.c ./../../c_src/vex/src/process_qstring.c \
 ./../../c_src/vex/src/read_file.c ./../../c_src/vex/src/scan_info.c \
 ./../../c_src/vex/src/strip_text.c ./../../c_src/vex/src/svex_info.c \
 ./../../c_src/vex/src/vex_init.c ./../../c_src/vex/src/write_vexfile.c

libvexpyb_la_LIBADD = ../../c_src/mk4util/libmk4utilb.la

libvexpyb_la_LDFLAGS = -avoid-version

SRC_FILES = vexpy/__init__.py \
	vexpy/vexpy.py

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
		--record $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_vexpy_files.txt \
		--verbose)

uninstall-local:
	cat $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_vexpy_files.txt | xargs rm -rf
	rm -rf $(DESTDIR)$(pythondir)/$(PKG_NAME)/install_vexpy_files.txt

CLEANFILES = $(SRC_FILES) ./include/vexpy_module_extern.h ./src/vexpy_module_extern.c setup.py.in

DISTCLEANFILES = setup.py

endif

#
# eof
#
