#
# $Id: Makefile.am 3399 2021-10-26 20:05:16Z barrettj $
#

SUBDIRS = .

AM_CPPFLAGS = @HOPS_INC@

noinst_HEADERS = adump.h

bin_PROGRAMS = adump

adump_SOURCES = adump.c parse_cmdline.c parse_bfields.c parse_tfields.c \
		strip_bline.c strip_tline.c write_header.c
adump_LDADD = @MSG_LIB@ @AFIO_LIB@ @UTIL_LIB@
adump_DEPENDENCIES = @MSG_DEP@ @AFIO_DEP@ @UTIL_DEP@

#
# eof
#
