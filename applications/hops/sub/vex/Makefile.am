#
# $Id: Makefile.am 338 2011-06-13 17:40:35Z gbc $
#

SUBDIRS = .

AM_CPPFLAGS = -I$(top_srcdir)/include

nobase_dist_pkgdata_DATA = text/blk_stmt.txt text/pformat.txt

pkginclude_HEADERS =

pkglib_LIBRARIES = libvex.a

libvex_a_SOURCES =  block_params.c do_source.c locate_cq.c \
		    check_stloc.c do_track.c nextchar.c decode_pval.c \
		    fill_deflists.c param_formats.c do_antenna.c \
		    fill_scanlist.c parse_date.c do_bbc.c \
		    fill_station_parms.c parse_pval.c do_clock.c \
		    find_statements.c parse_ref.c do_das.c get_def.c \
		    parse_units.c do_eop.c get_global_deflist.c \
		    parse_vexfile.c do_exper.c get_mode_deflist.c \
		    print_location.c do_freq.c get_statement.c \
		    read_file.c do_head_pos.c get_station_deflist.c \
		    scan_info.c do_if.c get_val_list.c strip_text.c \
		    do_pass_order.c in_comment.c \
		    do_phase_cal_detect.c in_quote.c vex_init.c \
		    do_roll.c init_scan.c write_vexfile.c do_site.c \
		    locate_blocks.c parse_ra.c parse_dec.c \
		    process_qstring.c check_scan.c \
		    get_corr_bd_parms.c cvex_info.c get_vex.c \
		    get_section_mode.c get_corr_mode.c \
		    get_chip_mode.c get_block_mode.c check_intrange.c \
		    check_realrange.c check_strrange.c evex_info.c \
		    get_su_connect.c get_drive_init.c \
		    get_trm_config.c get_pbs_init.c get_version.c \
		    ivex_info.c get_pcm_config.c get_pcm_tables.c \
		    get_su_chan_out.c svex_info.c lvex_info.c \
		    get_logscan.c 

#
# eof
#
