
MAINCODE = $(top_srcdir)/src/

check_PROGRAMS = configuration_test sysutil_test

configuration_test_SOURCES = configuration_test.cpp \
	$(MAINCODE)/alert.cpp \
	$(MAINCODE)/configuration.cpp \
	$(MAINCODE)/configurationstorage.cpp \
	$(MAINCODE)/mode.cpp \
	$(MAINCODE)/model.cpp \
	$(MAINCODE)/mk5mode.cpp \
	$(MAINCODE)/mathutil.cpp \
	$(MAINCODE)/polyco.cpp \
	$(MAINCODE)/sysutil.cpp \
	$(MAINCODE)/pcal.cpp

configuration_test_CXXFLAGS = -g -I../src/ $(AM_CXXFLAGS)

sysutil_test_SOURCES = sysutil_test.cpp \
	$(MAINCODE)/alert.cpp \
	$(MAINCODE)/sysutil.cpp

sysutil_test_CXXFLAGS = -g -I../src/ $(AM_CXXFLAGS)
