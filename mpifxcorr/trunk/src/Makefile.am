if WITH_SS
mark5_files = \
	nativemk5.cpp \
	mark5dir.cpp
else
mark5_files = \
	nativemk5_stubs.cpp
endif

bin_PROGRAMS = mpifxcorr
mpifxcorr_SOURCES = \
	mpifxcorr.cpp \
	fxmanager.cpp \
	core.cpp \
	datastream.cpp \
	visibility.cpp \
	configuration.cpp \
	uvw.cpp \
	mode.cpp \
	mk5.cpp \
	polyco.cpp \
	alert.cpp \
	$(mark5_files)

noinst_HEADERS = \
	fxmanager.h \
	core.h \
	datastream.h \
	architecture.h \
	visibility.h \
	configuration.h \
	uvw.h \
	mk5.h \
	polyco.h \
	mark5dir.h \
	nativemk5.h \
	alert.h

mpifxcorr_LDADD = \
	-lipps -lguide -lippvm 

lib_LIBRARIES = libmpifxcorr.a
libmpifxcorr_a_SOURCES = \
	configuration.cpp \
	mode.cpp \
	core.cpp \
	datastream.cpp \
	polyco.cpp \
	mk5.cpp \
	fxmanager.cpp \
	uvw.cpp \
	visibility.cpp \
	alert.cpp
