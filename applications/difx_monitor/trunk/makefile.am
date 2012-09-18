# automake makefile

bin_PROGRAMS = monitor_server mon_sample difx_config vcal

bin_SCRIPTS = stripinput.pl
EXTRA_DIST = $(bin_SCRIPTS)

noinst_HEADERS = monserver.h

check_PROGRAMS = difx_monitor

CXX = $(MPICXX)
CXXFLAGS = -g -Wall -D__PWD=\"$$PWD\" $(AM_CXXFLAGS)
INCLUDES = -I$(DIFXROOT)/include

difx_monitor_SOURCES = difx_monitor.cpp monserver.cpp
#difx_monitor_LDADD = $(PGPLOTLIBS)

monitor_server_SOURCES = monitor_server.cpp monserver.cpp

mon_sample_SOURCES = mon_sample.cpp monserver.cpp
#mon_sample_LDADD = $(PGPLOTLIBS)

difx_config_SOURCES = difx_config.cpp

vcal_SOURCES = vcal.cpp monserver.cpp

# eof
