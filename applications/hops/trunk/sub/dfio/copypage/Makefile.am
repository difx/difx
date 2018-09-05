#
# $Id: Makefile.am 2458 2018-07-19 21:30:23Z gbc $
# 

SUBDIRS = .

TESTS = fplot_test
TESTS_ENVIRONMENT = srcdir=$(srcdir) GS_DEBUG='gs_debug.ps' FP_SLEEP='-4'

AM_CPPFLAGS = -I$(top_srcdir)/include -DGS_EXEC=\"@GS_EXEC@\" \
		-DGS_COPYPAGE_OK=@GS_COPYPAGE_OK@

fplot_test_SOURCES = fplot_test.c
fplot_test_LDADD = -L.. -ldfio @UTIL_LIB@

check_PROGRAMS = fplot_test

CLEANFILES = gs_debug.ps

#
# eof
#
