#
# $Id: Makefile.am 3399 2021-10-26 20:05:16Z barrettj $
# 

SUBDIRS = .

TESTS = fplot_test
TESTS_ENVIRONMENT = srcdir=$(srcdir) GS_DEBUG='gs_debug.ps' FP_SLEEP='-4'

AM_CPPFLAGS = -I$(top_srcdir)/include -DGS_EXEC=\"@GS_EXEC@\" \
		-DGS_COPYPAGE_OK=@GS_COPYPAGE_OK@

fplot_test_SOURCES = fplot_test.c
fplot_test_LDADD = -L.. -ldfio @MSG_LIB@ @UTIL_LIB@

check_PROGRAMS = fplot_test

CLEANFILES = gs_debug.ps

#
# eof
#
