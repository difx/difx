INCLUDES = \
	-I$(top_srcdir)/difxmessage

LDADD = \
	$(top_builddir)/difxmessage/libdifxmessage.la

bin_PROGRAMS = \
	testdifxmessagesend \
	testdifxmessagereceive \
	loadmon

dist_bin_SCRIPTS = \
	mk5mon \
	cpumon

testdifxmessagesend_SOURCES = \
	testdifxmessagesend.c

testdifxmessagereceive_SOURCES = \
	testdifxmessagereceive.c

loadmon_SOURCES = \
	loadmon.c
