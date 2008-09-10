bin_PROGRAMS = mpifxcorr
mpifxcorr_SOURCES = mpifxcorr.cpp fxmanager.cpp core.cpp datastream.cpp \
	visibility.cpp configuration.cpp uvw.cpp mode.cpp mk5.cpp polyco.cpp

# set the include path found by configure
INCLUDES = -I/home/ssi/adeller//correlator/rpfits/code/ \
	-I/home/ssi/adeller/correlator/phase1fx/vlba_utils/ -I/usr/local/intel/ipp41/ia32_itanium/include/ $(all_includes)

# the library search path.
mpifxcorr_LDFLAGS = $(all_libraries) 
noinst_HEADERS = fxmanager.h core.h datastream.h architecture.h visibility.h \
	configuration.h uvw.h mk5.h polyco.h

mpifxcorr_LDADD = -L/usr/local/intel/ipp41/ia32_itanium/sharedlib/linux32/ \
	-L/usr/local/intel/ipp41/ia32_itanium/sharedlib/ -L/home/ssi/adeller//correlator/rpfits/linux/ \
	-L/home/ssi/adeller/correlator/phase1fx/vlba_utils/ -lipps -lguide -lippvm -lvlba_utils -lrpfits -lg2c

lib_LIBRARIES = libmpifxcorr.a
libmpifxcorr_a_SOURCES = configuration.cpp mode.cpp core.cpp datastream.cpp polyco.cpp \
	mk5.cpp fxmanager.cpp uvw.cpp visibility.cpp`
