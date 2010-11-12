bin_PROGRAMS = \
	transient_wrapper

AM_CXXFLAGS = $(DIFXMESSAGE_CFLAGS) $(DIFXIO_CFLAGS)

transient_wrapper_LDADD = $(DIFXMESSAGE_LIBS) $(DIFXIO_LIBS) -lpthread

transient_wrapper_SOURCES = \
	transient_wrapper.cpp \
	transient_wrapper_data.cpp \
	monitor.cpp \
	transient_wrapper_data.h
