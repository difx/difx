#
# $Id: Makefile.am 3399 2021-10-26 20:05:16Z barrettj $
#

SUBDIRS = .

AM_CPPFLAGS = @HOPS_INC@

noinst_HEADERS = average.h

bin_PROGRAMS = average

average_SOURCES = assess_data.c check_source.c get_int_time.c \
		  parse_cmdline.c sorter.c average.c check_times.c \
		  read_data.c triangle_avg.c baseline_avg.c \
		  check_tribase.c index_data.c sort_data.c 

average_LDADD = @MSG_LIB@ @AFIO_LIB@ @UTIL_LIB@
average_DEPENDENCIES = @MSG_DEP@ @AFIO_DEP@ @UTIL_DEP@

#
# eof
#
