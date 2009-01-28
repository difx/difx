INCLUDES = \
	-I$(top_srcdir)/difxmessage

LDADD = \
	$(top_builddir)/difxmessage/libdifxmessage.la

bin_PROGRAMS = \
	testdifxmessagesend \
	testdifxmessagereceive \
	difxlog

dist_bin_SCRIPTS = \
	mk5mon \
	cpumon \
	errormon \
	mk5display

testdifxmessagesend_SOURCES = \
	testdifxmessagesend.c

testdifxmessagereceive_SOURCES = \
	testdifxmessagereceive.c

difxlog_SOURCES = \
	difxlog.c
