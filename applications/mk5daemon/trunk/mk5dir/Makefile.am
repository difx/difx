if WITH_STREAMSTOR
bin_PROGRAMS = \
	fileto5c \
	mk5cat \
	mk5cp \
	mk5dir \
	mk5erase \
	mk5lock \
	mk5putdir \
	mk5map \
	mk5test \
	record5c \
	recover \
	testmod \
	vsn
else
bin_PROGRAMS = 
endif

dist_bin_SCRIPTS = \
	checkdir

fileto5c_SOURCES = \
	watchdog.cpp \
	watchdog.h \
	mark5dir.cpp \
	mark5dir.h \
	mark5directorystructs.h \
	fileto5c.cpp

mk5cat_LDADD = \
	${M5ACCESS_LIBS}

mk5cp_LDADD = \
	${M5ACCESS_LIBS}

mk5dir_LDADD = \
	${M5ACCESS_LIBS}

mk5erase_LDADD = \
	${M5ACCESS_LIBS}

mk5map_LDADD = \
	${M5ACCESS_LIBS}

recover_LDADD = \
	${M5ACCESS_LIBS}

mk5test_LDADD = \
	${M5ACCESS_LIBS}

testmod_LDADD = \
	${M5ACCESS_LIBS}

vsn_LDADD = \
	${M5ACCESS_LIBS}


mk5cat_SOURCES = \
	watchdog.cpp \
	watchdog.h \
	mark5dir.cpp \
	mark5dir.h \
	mark5directorystructs.h \
	mk5cp.cpp

mk5cp_SOURCES = \
	watchdog.cpp \
	watchdog.h \
	mark5dir.cpp \
	mark5dir.h \
	mark5directorystructs.h \
	mk5cp.cpp

mk5dir_SOURCES = \
	watchdog.cpp \
	watchdog.h \
	mark5dir.cpp \
	mark5dir.h \
	mark5directorystructs.h \
	mk5dir.cpp

mk5erase_SOURCES = \
	watchdog.cpp \
	watchdog.h \
	mark5dir.cpp \
	mark5dir.h \
	mark5directorystructs.h \
	mk5erase.cpp

mk5putdir_SOURCES = \
	watchdog.cpp \
	watchdog.h \
	mark5dir.cpp \
	mark5dir.h \
	mark5directorystructs.h \
	mk5putdir.cpp

mk5lock_SOURCES = \
	mk5lock.cpp

mk5map_SOURCES = \
	watchdog.cpp \
	watchdog.h \
	mark5dir.cpp \
	mark5dir.h \
	mark5directorystructs.h \
	mk5map.cpp

record5c_SOURCES = \
	watchdog.cpp \
	watchdog.h \
	mark5dir.cpp \
	mark5dir.h \
	mark5directorystructs.h \
	record5c.cpp

recover_SOURCES = \
	watchdog.cpp \
	watchdog.h \
	mark5dir.cpp \
	mark5dir.h \
	mark5directorystructs.h \
	recover.cpp

mk5test_SOURCES = \
	watchdog.cpp \
	watchdog.h \
	mk5test.cpp

testmod_SOURCES = \
	watchdog.cpp \
	watchdog.h \
	mark5dir.cpp \
	mark5dir.h \
	mark5directorystructs.h \
	testmod.cpp

vsn_SOURCES = \
	watchdog.cpp \
	watchdog.h \
	mark5dir.cpp \
	mark5dir.h \
	mark5directorystructs.h \
	nraobaddrives.cpp \
	nraobaddrives.h \
	vsn.cpp
