#
# $Id: Makefile.am 3399 2021-10-26 20:05:16Z barrettj $
#

SUBDIRS = .

AM_CPPFLAGS = @HOPS_INC@

noinst_HEADERS = fringex.h

bin_PROGRAMS = fringex
check_PROGRAMS =

fringex_c = accum_segs.c clear_fxp.c fringex.c \
		  read_binaries.c calc_seg.c model.c fill_aline.c realloc_segs.c \
		  parse_cmdline.c set_reffreq.c parse_iflag.c init_fxp.c \
		  parse_dflag.c clear_loops.c filelist.c set_loops.c determine_nsegs.c
# dms_to_deg.c

fringex_CPPFLAGS = -DBIGGER=1 $(AM_CPPFLAGS)
fringex_SOURCES = $(fringex_c)

fringex_LDADD = @MSG_LIB@ @DFIO_LIB@ @AFIO_LIB@ @UTIL_LIB@ @VEX_LIB@
fringex_DEPENDENCIES = @MSG_DEP@ @DFIO_DEP@ @AFIO_DEP@ @UTIL_DEP@ @VEX_DEP@

#
# eof
#
