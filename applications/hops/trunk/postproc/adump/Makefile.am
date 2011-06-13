#
# $Id: Makefile.am 265 2011-05-20 15:39:47Z gbc $
#

SUBDIRS = .

AM_CPPFLAGS = @HOPS_INC@

noinst_HEADERS = adump.h

bin_PROGRAMS = adump

adump_SOURCES = adump.c parse_cmdline.c parse_bfields.c parse_tfields.c \
		strip_bline.c strip_tline.c write_header.c
adump_LDADD = @AFIO_LIB@ @UTIL_LIB@
adump_DEPENDENCIES = @AFIO_DEP@ @UTIL_DEP@

#
# eof
#
