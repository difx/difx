#
# $Id$
#

SUBDIRS = .

AM_CPPFLAGS = -fPIC -I$(srcdir) \
	-I$(srcdir)/../msg \
	-I$(srcdir)/../dfio \
	-I$(srcdir)/../afio \
	-I$(srcdir)/../util \
	-I$(top_srcdir)/include

# Hacks for now until we work out exactly how to use libtool
# this may work:
# pyexec_LTLIBRARIES = $(libdfio) $(libafio) $(libutil)
pkglib_LTLIBRARIES = libmk4io.la

pkgpython_PYTHON = mk4.py difxio.py afio.py

libmsg = msg.c

libdfio = \
	init_000.c addr_100.c copy_100.c clear_100.c \
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
	clear_221.c copy_222.c clear_222.c addr_222.c \
	addr_230.c copy_230.c clear_230.c \
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

libafio = \
	afile_header.c clear_csumm.c clear_rsumm.c \
	corelname.c get_unique_name.c parse_fsumm.c \
	parse_tsumm.c write_csumm.c write_rsumm.c \
	aline_id.c check_sizes.c clear_fsumm.c \
	clear_tsumm.c fringename.c parse_csumm.c \
	parse_rsumm.c rootname.c write_fsumm.c write_tsumm.c \
	clear_afile_structure.c read_afile.c

libutil = \
	check_name.c datec_to_datef.c extract_filenames.c  \
	root_belong.c datef_to_datec.c  \
	get_filelist.c root_id.c confirm.c day_of_datef.c  \
	sort_names.c hptoie.c hptoie4.c hptoie8.c  \
	time_to_int.c int_to_time.c environment.c  \
	minmax.c account.c report_times.c syntax.c  \
	clear_date.c swap.c fileset.c ismk4.c  \
	time_to_double.c get_abs_path.c

# this is really crude for the moment
# it brings in copies of the required sub/*/*c targets that we
# need to build the libmk4io shared library target
mk4timestamp.c: $(srcdir)/Makefile.am force
	-cp -p $(srcdir)/../msg/*.c .
	-cp -p $(srcdir)/../dfio/*.c .
	-cp -p $(srcdir)/../afio/*.c .
	-cp -p $(srcdir)/../util/*.c .
	-echo 'char mk4py_timestamp[75] = "' `date -u` '";' > mk4timestamp.c

force:
	echo 'forcing a copy operation'

# automake rules seem to require this
libmk4io.c:
	echo 'char mk4_totalhack[] = "should use libtool";' > libmk4io.c

BUILT_SOURCES = mk4timestamp.c $(libmsg) $(libdfio) $(libafio) $(libutil)

libmk4io_la_SOURCES = extern.c mk4timestamp.c $(libmsg) $(libdfio) $(libafio) $(libutil)

uninstall-local:
	rm -rf $(DESTDIR)$(pkgpythondir)

CLEANFILES = *.c

#
# eof
#
