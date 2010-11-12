INCLUDES = \
	-I$(top_srcdir)/difxmessage

LDADD = \
	$(top_builddir)/difxmessage/libdifxmessage.la \
	$(top_builddir)/mark5ipc/libmark5ipc.la

bin_PROGRAMS = \
	testdifxmessagesend \
	testdifxmessagereceive \
	testdifxmessagecondition \
	testdifxmessagereceivecond \
	testseqnumbers \
	testm5lock \
	sendtransient \
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

testdifxmessagecondition_SOURCES = \
	testdifxmessagecondition.c

testseqnumbers_SOURCES = \
	testseqnumbers.c

testdifxmessagereceivecond_SOURCES = \
	testdifxmessagereceivecond.c

sendtransient_SOURCES = \
	sendtransient.c

difxlog_SOURCES = \
	difxlog.c

testm5lock_SOURCES = \
	testm5lock.c
