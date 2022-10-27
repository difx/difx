#
# $Id: Makefile.am 3399 2021-10-26 20:05:16Z barrettj $
#

SUBDIRS = .

AM_CPPFLAGS = @HOPS_INC@

bin_PROGRAMS = alist

alist_SOURCES = alist.c parse_cmdline.c open_output.c summarize_mk4fringe.c 

alist_LDADD = @MSG_LIB@ @DFIO_LIB@ @AFIO_LIB@ @UTIL_LIB@
alist_DEPENDENCIES = @MSG_DEP@ @DFIO_DEP@ @AFIO_DEP@ @UTIL_DEP@

#
# eof
#
