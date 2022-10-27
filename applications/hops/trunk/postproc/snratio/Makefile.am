#
# $Id: Makefile.am 436 2011-08-29 14:05:02Z rjc $
#

SUBDIRS = .

AM_CPPFLAGS = @HOPS_INC@

bin_PROGRAMS = snratio

snratio_SOURCES = snratio.c

snratio_LDADD = @MSG_LIB@ @AFIO_LIB@ @UTIL_LIB@
snratio_DEPENDENCIES = @MSG_DEP@ @AFIO_DEP@  @UTIL_DEP@

#
# eof
#
