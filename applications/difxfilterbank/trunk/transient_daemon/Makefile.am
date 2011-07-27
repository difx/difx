bin_PROGRAMS = \
	transient_daemon \
	transient_daemon_control

AM_CXXFLAGS = $(DIFXMESSAGE_CFLAGS) $(DIFXIO_CFLAGS)

transient_daemon_LDADD = $(DIFXMESSAGE_LIBS) $(DIFXIO_LIBS)

transient_daemon_control_LDADD = $(DIFXMESSAGE_LIBS)

transient_daemon_SOURCES = \
	transient_daemon.cpp \
	recorr.cpp \
	recorr.h

transient_daemon_control_SOURCES = \
	transient_daemon_control.cpp
