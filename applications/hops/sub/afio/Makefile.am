#
# $Id: Makefile.am 1461 2016-12-13 18:57:49Z barrettj $
#

SUBDIRS = .

TESTS = check_sizes

AM_CPPFLAGS = -I$(top_srcdir)/include

pkginclude_HEADERS =

pkglib_LIBRARIES = libafio.a

libafio_a_SOURCES = clear_rsumm.c clear_csumm.c clear_fsumm.c \
		    parse_rsumm.c parse_csumm.c parse_fsumm.c \
		    write_rsumm.c write_csumm.c write_fsumm.c \
		    clear_tsumm.c write_tsumm.c parse_tsumm.c \
		    rootname.c corelname.c fringename.c \
		    get_unique_name.c aline_id.c afile_header.c \
		    clear_afile_structure.c read_afile.c

check_PROGRAMS = check_sizes
check_sizes_SOURCES = check_sizes.c

#
# eof
#
