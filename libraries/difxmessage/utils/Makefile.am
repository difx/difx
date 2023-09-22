AM_CPPFLAGS = \
	-I$(top_srcdir)

LDADD = \
	$(top_builddir)/difxmessage/libdifxmessage.la \
	$(top_builddir)/mark5ipc/libmark5ipc.la

bin_PROGRAMS = \
	testdifxmessagesend \
	testdifxmessagereceive \
	testdifxmessagereceivecond \
	testdifxmessagedrivestats \
	testseqnumbers \
	testm5lock \
	sendtransient \
	sendsmart \
	updatedifxclock \
	difxlog \
	probemachine

dist_bin_SCRIPTS = \
	cpumon \
	getsmart \
	errormon \
	errormon2 \
	mk5mon \
	smartmon \
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

testdifxmessagereceivecond_SOURCES = \
	testdifxmessagereceivecond.c

sendtransient_SOURCES = \
	sendtransient.c

sendsmart_SOURCES = \
	sendsmart.c

updatedifxclock_SOURCES = \
	updatedifxclock.c

difxlog_SOURCES = \
	difxlog.c

testm5lock_SOURCES = \
	testm5lock.c

testdifxmessagedrivestats_SOURCES = \
	testdifxmessagedrivestats.c

