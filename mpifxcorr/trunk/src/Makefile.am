if WITH_SS
mark5_files = \
	mark5utils.cpp \
	nativemk5.cpp \
	mark5bmark5.cpp \
	vdifmark5.cpp \
	mark5dir.cpp \
	mark5directorystructs.cpp \
	watchdog.cpp
else
mark5_files = \
	nativemk5_stubs.cpp \
	mark5bmark5_stubs.cpp \
	vdifmark5_stubs.cpp
endif

bin_PROGRAMS = mpifxcorr neuteredmpifxcorr

mpifxcorr_SOURCES = \
	mpifxcorr.cpp \
	fxmanager.cpp \
	core.cpp \
	datastream.cpp \
	visibility.cpp \
	configuration.cpp \
	mathutil.cpp \
	mode.cpp \
        model.cpp \
	mk5.cpp \
	mk5mode.cpp \
	datamuxer.cpp \
	mark5bfile.cpp \
	vdiffile.cpp \
	vdiffake.cpp \
	vdifnetwork.cpp \
	polyco.cpp \
	alert.cpp \
	pcal.cpp \
	switchedpower.cpp \
	vdifmark6_datastream.cpp \
	$(mark5_files)

library_includedir = $(includedir)/mpifxcorr
library_include_HEADERS = \
	fxmanager.h \
	pcal.h \
	core.h \
	datastream.h \
	architecture.h \
	visibility.h \
	configuration.h \
	mathutil.h \
	mk5.h \
	mk5mode.h \
        model.h \
        mode.h \
	polyco.h \
	mark5dir.h \
	nativemk5.h \
	watchdog.h \
	mark5utils.h \
	mpifxcorr.h \
	switchedpower.h \
	datamuxer.h \
	mark5bfile.h \
	vdiffile.h \
	vdiffake.h \
	vdifnetwork.h \
	alert.h 

# historically these have been in both $(includedir)/{.,mpifxcorr}
include_HEADERS = $(library_include_HEADERS)

lib_LIBRARIES = libmpifxcorr.a libfxcorr.a
libmpifxcorr_a_SOURCES = \
	pcal.cpp \
	configuration.cpp \
	mode.cpp \
	core.cpp \
	datastream.cpp \
	polyco.cpp \
	mk5.cpp \
	mk5mode.cpp \
	fxmanager.cpp \
	mathutil.cpp \
        model.cpp \
	visibility.cpp \
	alert.cpp \
	switchedpower.cpp \
	mark5bfile.cpp \
	vdiffile.cpp \
	vdiffake.cpp \
	vdifnetwork.cpp \
	datamuxer.cpp \
	vdifmark6_datastream.cpp \
	$(mark5_files)

libfxcorr_a_SOURCES = \
	configuration.cpp \
	pcal.cpp \
	mathutil.cpp \
	mode.cpp \
	mk5mode.cpp \
	polyco.cpp \
	visibility.cpp \
        model.cpp \
	datamuxer.cpp \
	alert.cpp

neuteredmpifxcorr_SOURCES = \
	mpifxcorr.cpp \
	fxmanager.cpp \
	core.cpp \
	datastream.cpp \
	mathutil.cpp \
	mk5.cpp \
	switchedpower.cpp \
	mark5bfile.cpp \
	vdiffile.cpp \
	vdiffake.cpp \
	vdifnetwork.cpp \
	datamuxer.cpp \
	vdifmark6_datastream.cpp \
	$(mark5_files)

neuteredmpifxcorr_CXXFLAGS = -DNEUTERED_DIFX $(AM_CXXFLAGS)

neuteredmpifxcorr_LDADD = libfxcorr.a
