#
# $Id: Makefile.am 3399 2021-10-26 20:05:16Z barrettj $
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

fourmer_LDADD = @MSG_LIB@ @AFIO_LIB@ @DFIO_LIB@ @UTIL_LIB@
fourmer_DEPENDENCIES = @MSG_DEP@ @AFIO_DEP@ @DFIO_DEP@ @UTIL_DEP@

test_new_chan_id_SOURCES = test_new_chan_id.c gen_new_chan_id.c

#
# eof
#
