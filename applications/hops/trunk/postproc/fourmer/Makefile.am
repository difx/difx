#
# $Id: Makefile.am 260 2011-05-19 21:10:14Z gbc $
#

SUBDIRS = .

AM_CPPFLAGS = @HOPS_INC@

TESTS_ENVIRONMENT = srcdir=$(srcdir)
TESTS             = test_new_chan_id

noinst_HEADERS = fourmer.h
bin_PROGRAMS = fourmer
check_PROGRAMS = test_new_chan_id

fourmer_SOURCES = fourmer.c do_record_merge.c gen_new_chan_id.c \
		  append_sdata.c scan_name_edit.c print_cdata_cmp.c

fourmer_LDADD = @AFIO_LIB@ @DFIO_LIB@ @UTIL_LIB@
fourmer_DEPENDENCIES = @AFIO_DEP@ @DFIO_DEP@ @UTIL_DEP@

test_new_chan_id_SOURCES = test_new_chan_id.c gen_new_chan_id.c

#
# eof
#
