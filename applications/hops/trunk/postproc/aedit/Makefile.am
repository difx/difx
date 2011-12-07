#
# $Id: Makefile.am 436 2011-08-29 14:05:02Z rjc $
#

SUBDIRS = .

AM_CPPFLAGS = @HOPS_INC@

noinst_HEADERS = aedata.h aedit.h close_flags.h flags.h \
		 param_list.h psplot.h pstruct.h sizelimits.h \
		 sort.h summary.h tempo.h usearray.h

bin_PROGRAMS = aedit

if HAVE_PGPLOT
if HAVE_X11
if HAVE_PNG

aedit_SOURCES = active_filter.c aeclr_corel.c aeclr_fringe.c \
		aeclr_root.c aedit.c allocate_parms.c axis.c \
		axis_scale.c calc_close.c cfilter.c check_env.c \
		cleanup.c cleanup_psplot.c clear.c \
		clear_pstruct.c clear_source.c triangle_value.c \
		command_id.c cursor_select.c datum_value.c \
		display_psdata.c dup_check.c dup_flag.c edit.c \
		erase_point.c esdesp_check.c execute.c \
		extract_parms.c ffilter.c fill_closure.c \
		fplot.c get_axis.c get_param.c \
		get_param_list.c get_plot_data.c get_plot_datum.c \
		help.c init_inputs.c locate_pscurs.c make_tri.c \
		makekey.c param_list.c parse_commands.c plot.c \
		plot_fqex.c plot_header.c plot_points.c \
		plot_quality.c pr_allsumm.c pr_csumm.c pr_fsumm.c \
		pr_inputs.c pr_rsumm.c pr_source.c pr_ssumm.c \
		pr_summary.c print_data.c ps_baselabel.c \
		ps_fplot.c ps_free.c ps_inside.c ps_proc_datum.c \
		ps_scanlabel.c ps_selbase.c ps_selqual.c \
		ps_selscan.c psplot.c pstag_process.c \
		read_cursor.c read_data.c rfilter.c \
		run_com_file.c run_pscursor.c set_baselines.c \
		set_fraction.c set_frequencies.c set_mode.c \
		set_nfreq.c set_prange.c set_procrange.c \
		set_pscodes.c set_psparam.c set_qcodes.c \
		set_sources.c set_stations.c set_timerange.c \
		set_type.c set_year.c setup_plot.c setup_psplot.c \
		smatch.c sorter.c summ_corel.c summ_data.c \
		summ_fringe.c summ_root.c symbol_key.c \
		trngl_present.c unflag.c update_fqex.c \
		update_sinfo.c write_data.c write_param.c \
		write_prmline.c write_psfile.c zoom.c time_axis.c \
		set_device.c auto_hardcopy.c edit_families.c \
		cross_link.c test1.c write_families.c \
		station_reduce.c aeclr_triangle.c aeclr_quad.c \
		tarray_index.c qarray_index.c write_tdata.c \
		clear_summ.c alloc_btq.c fill_tdata.c \
		clear_fqex.c summ_triangle.c summ_quad.c \
		init_summ.c pr_tsumm.c set_triangles.c \
		set_quads.c tfilter.c qfilter.c save.c tricheck.c \
		edit_close.c parse_cmdline.c write_pshdr.c \
		add_station.c mk3_qf.c fill4_parms.c psfile4.c \
		write_reproc.c get_ps_indices4.c \
		psplot_defaults4.c make_psarray4.c psplot4.c \
		set_pols.c

# original order:
#aedit_LDADD = @PGP_LIB@ @PNG_LIB@ @X_FPLOT_LIB@ \
#	      @VEX_LIB@ @AFIO_LIB@ @DFIO_LIB@ @UTIL_LIB@ @X11_LIB@
# more logical order:
aedit_LDADD = @VEX_LIB@ @AFIO_LIB@ @DFIO_LIB@ @UTIL_LIB@ \
	      @PGP_LIB@ @PNG_LIB@ @X_FPLOT_LIB@ @X11_LIB@
aedit_DEPENDENCIES = @VEX_DEP@ @AFIO_DEP@ @DFIO_DEP@ @UTIL_DEP@

else !HAVE_PNG
aedit_SOURCES = you_lose.c
endif !HAVE_PNG
else !HAVE_X11
aedit_SOURCES = you_lose.c
endif !HAVE_X11
else !HAVE_PGPLOT
aedit_SOURCES = you_lose.c
endif !HAVE_PGPLOT

you_lose.c: Makefile.am
	echo 'main(){return(puts("Missing PNG, X11 or PGPLOT\n"));}' > $@

CLEANFILES = you_lose.c

#
# eof
#
