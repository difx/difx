bin_PROGRAMS = checkmpifxcorr dedisperse_difx

AM_CXXFLAGS = -I$(srcdir)/../src/

checkmpifxcorr_SOURCES = \
	checkmpifxcorr.cpp

dedisperse_difx_SOURCES = \
	dedisperse_difx.cpp

checkmpifxcorr_LDADD = ../src/libmpifxcorr.a

dedisperse_difx_LDADD = ../src/libmpifxcorr.a
