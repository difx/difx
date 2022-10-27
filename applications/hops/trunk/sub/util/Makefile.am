#
# $Id: Makefile.am 3453 2021-11-17 21:53:29Z gbc $
#

SUBDIRS = .

TESTS = test_root_id test_syntax test_vextext
TESTS_ENVIRONMENT = srcdir=$(srcdir)

AM_CPPFLAGS = -I$(top_srcdir)/include \
	-DHOPS_VEX_TEXT_SHARE_DIR='"$(pkgdatadir)/text"' \
 	-DHOPS_VEX_TEXT_SRC_DIR='"$(abs_top_srcdir)/sub/vex/text"'

pkginclude_HEADERS =

pkglib_LIBRARIES = libmk4util.a

libmk4util_a_SOURCES = check_name.c datec_to_datef.c extract_filenames.c  \
		       root_belong.c clear_fstruct.c datef_to_datec.c  \
		       get_filelist.c root_id.c confirm.c day_of_datef.c  \
		       sort_names.c hptoie.c hptoie4.c hptoie8.c  \
		       time_to_int.c int_to_time.c environment.c  \
		       minmax.c account.c report_times.c syntax.c  \
		       clear_date.c swap.c fileset.c ismk4.c  \
		       time_to_double.c get_abs_path.c sexigesimal2hrdeg.c \
		       adler32_checksum.c hops_complex.c

# the original test program
check_PROGRAMS = test_root_id test_syntax test_vextext

test_root_id_SOURCES = root_id_test.c
test_root_id_LDADD = -L. -lmk4util @MSG_LIB@

test_syntax_SOURCES = test_syntax.c
test_syntax_LDADD = -L. -lmk4util @MSG_LIB@

test_vextext_SOURCES = test_vextext.c
test_vextext_LDADD = -L. -lmk4util @MSG_LIB@

#
# eof
#
