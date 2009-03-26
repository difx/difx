INCLUDES = \
	-I$(top_srcdir)/difxmessage

LDADD = \
	$(top_builddir)/difxmessage/libdifxmessage.la

bin_PROGRAMS = \
	testdifxmessagesend \
	testdifxmessagereceive \
	testseqnumbers \
	difxlog

dist_bin_SCRIPTS = \
	mk5mon \
	cpumon \
	errormon \
	errormon2 \
	mk5display

testdifxmessagesend_SOURCES = \
	testdifxmessagesend.c

testdifxmessagereceive_SOURCES = \
	testdifxmessagereceive.c

testseqnumbers_SOURCES = \
	testseqnumbers.c

difxlog_SOURCES = \
	difxlog.c
