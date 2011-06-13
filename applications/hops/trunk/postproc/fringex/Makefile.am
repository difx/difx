#
# $Id: Makefile.am 288 2011-06-01 21:02:09Z gbc $
#

SUBDIRS = .

AM_CPPFLAGS = @HOPS_INC@

noinst_HEADERS = fringex.h

bin_PROGRAMS = fringex
check_PROGRAMS = fringey

fringex_c = accum_segs.c clear_fxp.c fringex.c \
		  read_binaries.c calc_seg.c model.c fill_aline.c \
		  parse_cmdline.c set_reffreq.c parse_iflag.c \
		  parse_dflag.c clear_loops.c filelist.c set_loops.c 

fringex_CPPFLAGS = -DBIGGER=1 $(AM_CPPFLAGS)
fringex_SOURCES = $(fringex_c)

fringey_CPPFLAGS = -DBIGGER=0 $(AM_CPPFLAGS)
fringey_SOURCES = $(fringex_c)

# dms_to_deg.c

fringex_LDADD = @DFIO_LIB@ @AFIO_LIB@ @UTIL_LIB@ @VEX_LIB@
fringex_DEPENDENCIES = @DFIO_DEP@ @AFIO_DEP@ @UTIL_DEP@ @VEX_DEP@

fringey_LDADD = @DFIO_LIB@ @AFIO_LIB@ @UTIL_LIB@ @VEX_LIB@
fringey_DEPENDENCIES = @DFIO_DEP@ @AFIO_DEP@ @UTIL_DEP@ @VEX_DEP@

#
# eof
#
