bin_PROGRAMS = mpifxcorr
mpifxcorr_SOURCES = mpifxcorr.cpp fxmanager.cpp core.cpp datastream.cpp \
	visibility.cpp configuration.cpp uvw.cpp mode.cpp mk5.cpp polyco.cpp

# set the include path found by configure
INCLUDES = -I$(DIFXROOT)/include/ -I$(IPPROOT)/include/ $(all_includes)

# the library search path.
mpifxcorr_LDFLAGS = $(all_libraries) 
noinst_HEADERS = fxmanager.h core.h datastream.h architecture.h visibility.h \
	configuration.h uvw.h mk5.h polyco.h

mpifxcorr_LDADD = -L$(IPPROOT)/sharedlib/ -L$(DIFXROOT)/lib/ \
	-lippsem64t -lippcoreem64t -lippvmem64t -lguide -lvlba_utils -lrpfits -lgfortran

lib_LIBRARIES = libmpifxcorr.a
libmpifxcorr_a_SOURCES = configuration.cpp mode.cpp core.cpp datastream.cpp polyco.cpp \
	mk5.cpp fxmanager.cpp uvw.cpp visibility.cpp`
