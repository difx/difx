#
# $Id: Makefile.am 802 2013-03-18 13:26:38Z gbc $
#

SUBDIRS = .

TESTS = test_compress test_mk4fringe
TESTS_ENVIRONMENT = srcdir=$(srcdir)

AM_CPPFLAGS = -I$(top_srcdir)/include -DGS_EXEC=\"@GS_EXEC@\"

pkginclude_HEADERS = bytflp.h

pkglib_LIBRARIES = libdfio.a

libdfio_a_SOURCES = init_000.c addr_100.c copy_100.c clear_100.c \
		    addr_101.c copy_101.c clear_101.c addr_120.c \
		    copy_120.c clear_120.c addr_200.c copy_200.c \
		    clear_200.c addr_201.c copy_201.c clear_201.c \
		    addr_202.c copy_202.c clear_202.c addr_203.c \
		    copy_203.c clear_203.c addr_204.c copy_204.c \
		    clear_204.c addr_205.c copy_205.c clear_205.c \
		    addr_206.c copy_206.c clear_206.c addr_207.c \
		    copy_207.c clear_207.c addr_208.c copy_208.c \
		    clear_208.c addr_210.c copy_210.c clear_210.c \
		    addr_212.c copy_212.c clear_212.c addr_220.c \
		    copy_220.c clear_220.c addr_221.c copy_221.c \
		    clear_221.c addr_230.c copy_230.c clear_230.c \
		    addr_300.c copy_300.c clear_300.c addr_301.c \
		    copy_301.c clear_301.c addr_302.c copy_302.c \
		    clear_302.c addr_303.c copy_303.c clear_303.c \
		    addr_304.c copy_304.c clear_304.c addr_305.c \
		    copy_305.c clear_305.c addr_306.c copy_306.c \
		    clear_306.c addr_307.c copy_307.c clear_307.c \
		    addr_308.c copy_308.c clear_308.c addr_309.c \
		    copy_309.c clear_309.c clear_mk4corel.c \
		    read_mk4corel.c write_mk4corel.c clear_mk4sdata.c \
		    read_mk4sdata.c write_mk4sdata.c open_datafile.c \
		    read_mk4file.c write_record.c corel_alloc.c \
		    write_err.c clear_coord.c write_mk4root.c \
		    write_mk4fringe.c read_mk4fringe.c \
		    clear_mk4fringe.c display_221.c compress.c

# the original test programs
check_PROGRAMS = test_mk4fringe test_compress

test_mk4fringe_SOURCES = test.c
test_mk4fringe_LDADD = -L. -ldfio @AFIO_LIB@ @UTIL_LIB@

test_compress_SOURCES = test_compress.c compress.c
test_compress_LDADD = 

CLEANFILES = fourmakes

#
# eof
#
