#
# $Id: Makefile.am 436 2011-08-29 14:05:02Z rjc $
#

SUBDIRS = .

AM_CPPFLAGS = @HOPS_INC@

bin_PROGRAMS = alist

alist_SOURCES = alist.c parse_cmdline.c open_output.c summarize_mk4fringe.c 

alist_LDADD = @DFIO_LIB@ @AFIO_LIB@ @UTIL_LIB@
alist_DEPENDENCIES = @DFIO_DEP@ @AFIO_DEP@ @UTIL_DEP@

#
# eof
#
