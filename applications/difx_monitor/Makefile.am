# automake makefile

if WITH_PLPLOT
plplotprograms = \
	mon_sample \
	difx_monitor

mon_sample_SOURCES = mon_sample.cpp monserver.cpp

difx_monitor_SOURCES = difx_monitor.cpp monserver.cpp
else
plplotprograms =
endif


bin_PROGRAMS = \
	$(plplotprograms) \
	monitor_server \
	vcal \
	difx_config

bin_SCRIPTS = stripinput.pl
EXTRA_DIST = $(bin_SCRIPTS)

noinst_HEADERS = monserver.h

check_PROGRAMS = difx_monitor

monitor_server_SOURCES = monitor_server.cpp monserver.cpp

difx_config_SOURCES = difx_config.cpp

vcal_SOURCES = vcal.cpp monserver.cpp
