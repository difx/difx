bin_PROGRAMS = checkmpifxcorr dedisperse_difx mpispeed

# pick up built sources as well
AM_CPPFLAGS = -I../src -I$(top_srcdir)/src

checkmpifxcorr_SOURCES = \
	checkmpifxcorr.cpp

dedisperse_difx_SOURCES = \
	dedisperse_difx.cpp

mpispeed_SOURCES = \
	mpispeed.cpp

checkmpifxcorr_LDADD = ../src/libmpifxcorr.a

dedisperse_difx_LDADD = ../src/libmpifxcorr.a
