#
# $Id: Makefile.am 1366 2016-07-20 20:07:44Z gbc $
#

SUBDIRS = .

TESTS = test_root_id
TESTS_ENVIRONMENT = srcdir=$(srcdir)

AM_CPPFLAGS = -I$(top_srcdir)/include

pkginclude_HEADERS =

pkglib_LIBRARIES = libmk4util.a

libmk4util_a_SOURCES = check_name.c datec_to_datef.c extract_filenames.c  \
		       root_belong.c clear_fstruct.c datef_to_datec.c  \
		       get_filelist.c root_id.c confirm.c day_of_datef.c  \
		       msg.c sort_names.c hptoie.c hptoie4.c hptoie8.c  \
		       time_to_int.c int_to_time.c environment.c  \
		       minmax.c account.c report_times.c syntax.c  \
		       clear_date.c swap.c fileset.c ismk4.c  \
		       time_to_double.c get_abs_path.c sexigesimal2hrdeg.c

# the original test program
check_PROGRAMS = test_root_id

test_root_id_SOURCES = test.c
test_root_id_LDADD = -L. -lmk4util

#
# eof
#
