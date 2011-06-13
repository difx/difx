#
# $Id: Makefile.am 265 2011-05-20 15:39:47Z gbc $
#

SUBDIRS = .

AM_CPPFLAGS = @HOPS_INC@

bin_PROGRAMS = alist

alist_SOURCES = alist.c parse_cmdline.c summarize_corel.c \
		summarize_fringe.c summarize_root.c open_output.c \
		summarize_mk4fringe.c 

alist_LDADD = @DFIO_LIB@ @AFIO_LIB@ @UTIL_LIB@
alist_DEPENDENCIES = @DFIO_DEP@ @AFIO_DEP@ @UTIL_DEP@

#
# eof
#
