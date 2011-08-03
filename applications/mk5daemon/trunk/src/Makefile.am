if WITH_STREAMSTOR
streamstor_sources = \
	modules.cpp \
	mark5a.cpp \
	smart.cpp \
	record.cpp \
	smart.h \
	ssversions.cpp
else
streamstor_sources = 
endif

bin_PROGRAMS = \
	mk5daemon

mk5daemon_SOURCES = \
	difx.cpp \
	loadmon.cpp \
	logger.cpp \
	logger.h \
	monitor.cpp \
	getdir.cpp \
	datacopy.cpp \
	condition.cpp \
	proc.cpp \
	proc.h \
	mk5daemon.cpp \
	mk5daemon.h \
	$(streamstor_sources)
	
