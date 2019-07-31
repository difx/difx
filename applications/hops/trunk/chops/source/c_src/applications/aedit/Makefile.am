#
# $Id$
#

SUBDIRS = .

AM_CPPFLAGS = -I. -I$(srcdir)/include -I$(srcdir)/../../mk4util/include -I$(srcdir)/../../afio/include -I$(srcdir)/../../dfio/include -I$(srcdir)/../../vex/include  -Wall -Wextra -DHAVE_CONFIG_H

AM_CPPFLAGS += ${FFTW3_CFLAGS} ${X_CFLAGS} -I${PGPLOT_DIR}

bin_PROGRAMS = baedit

pkginclude_HEADERS = \
    ./include/aedata.h \
    ./include/aedit.h \
    ./include/aedit_proto.h \
    ./include/close_flags.h \
    ./include/flags.h \
    ./include/param_list.h \
    ./include/psplot.h \
    ./include/pstruct.h \
    ./include/sizelimits.h \
    ./include/sort.h \
    ./include/summary.h \
    ./include/tempo.h \
    ./include/usearray.h


baedit_SOURCES = aedit.c src/active_filter.c src/add_station.c src/aeclr_corel.c src/aeclr_fringe.c src/aeclr_quad.c src/aeclr_root.c src/aeclr_triangle.c src/alloc_btq.c src/allocate_parms.c src/auto_hardcopy.c src/axis.c src/axis_scale.c src/calc_close.c src/cfilter.c src/check_env.c src/cleanup.c src/cleanup_psplot.c src/clear.c src/clear_fqex.c src/clear_pstruct.c src/clear_source.c src/clear_summ.c src/command_id.c src/cross_link.c src/cursor_select.c src/datum_value.c src/display_psdata.c src/dup_check.c src/dup_flag.c src/edit.c src/edit_close.c src/edit_families.c src/erase_point.c src/esdesp_check.c src/execute.c src/extract_parms.c src/ffilter.c src/fill4_parms.c src/fill_closure.c src/fill_tdata.c src/fplot.c src/get_axis.c src/get_param.c src/get_param_list.c src/get_plot_data.c src/get_plot_datum.c src/get_ps_indices4.c src/help.c src/init_inputs.c src/init_summ.c src/locate_pscurs.c src/make_psarray4.c src/make_tri.c src/makekey.c src/mk3_qf.c src/param_list.c src/parse_cmdline.c src/parse_commands.c src/plot.c src/plot_fqex.c src/plot_header.c src/plot_points.c src/plot_quality.c src/pr_allsumm.c src/pr_csumm.c src/pr_fsumm.c src/pr_inputs.c src/pr_rsumm.c src/pr_source.c src/pr_ssumm.c src/pr_summary.c src/pr_tsumm.c src/print_data.c src/ps_baselabel.c src/ps_fplot.c src/ps_free.c src/ps_inside.c src/ps_proc_datum.c src/ps_scanlabel.c src/ps_selbase.c src/ps_selqual.c src/ps_selscan.c src/psfile4.c src/psplot.c src/psplot4.c src/psplot_defaults4.c src/pstag_process.c src/qarray_index.c src/qfilter.c src/read_cursor.c src/read_data.c src/rfilter.c src/run_com_file.c src/run_pscursor.c src/save.c src/set_baselines.c src/set_device.c src/set_fraction.c src/set_frequencies.c src/set_mode.c src/set_nfreq.c src/set_pols.c src/set_prange.c src/set_procrange.c src/set_pscodes.c src/set_psparam.c src/set_qcodes.c src/set_quads.c src/set_sources.c src/set_stations.c src/set_timerange.c src/set_triangles.c src/set_type.c src/set_year.c src/setup_plot.c src/setup_psplot.c src/smatch.c src/sorter.c src/station_reduce.c src/summ_corel.c src/summ_data.c src/summ_fringe.c src/summ_quad.c src/summ_root.c src/summ_triangle.c src/symbol_key.c src/tarray_index.c src/test1.c src/tfilter.c src/time_axis.c src/triangle_value.c src/tricheck.c src/trngl_present.c src/unflag.c src/update_fqex.c src/update_sinfo.c src/write_data.c src/write_families.c src/write_param.c src/write_prmline.c src/write_psfile.c src/write_pshdr.c src/write_reproc.c src/write_tdata.c src/zoom.c

baedit_LDADD = ../../mk4util/libmk4utilb.la ../../afio/libafiob.la ../../dfio/libdfiob.la ../../vex/libvexb.la

baedit_LDADD += ${PGP_LIB} ${PNG_LIB} ${X_FPLOT_LIB} ${X11_LIB} ${FFTW3_LIBS}

check_SCRIPTS = ./import_aedit.sh
EXTRA_DIST = ./import_aedit.sh

#
# eof
#
