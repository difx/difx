AM_CPPFLAGS = \
	-I$(top_srcdir)

LDADD = \
	$(top_builddir)/difxmessage/libdifxmessage.la \
	$(top_builddir)/mark5ipc/libmark5ipc.la

bin_PROGRAMS = \
	testdifxmessagesend \
	testdifxmessagereceive \
	testseqnumbers \
	testm5lock \
	sendtransient \
	updatedifxclock \
	difxlog \
	probemachine

dist_bin_SCRIPTS = \
	cpumon \
	errormon \
	errormon2 \
	statemon \
	restartdifx \
	difxdiagnosticmon \
	difxwatch \
	mk6mon

probemachine_SOURCES = \
	probemachine.c \
	testmachine.c \
	testmachine.h

testdifxmessagesend_SOURCES = \
	testdifxmessagesend.c

testdifxmessagereceive_SOURCES = \
	testdifxmessagereceive.c

testseqnumbers_SOURCES = \
	testseqnumbers.c

sendtransient_SOURCES = \
	sendtransient.c

updatedifxclock_SOURCES = \
	updatedifxclock.c

difxlog_SOURCES = \
	difxlog.c

testm5lock_SOURCES = \
	testm5lock.c

