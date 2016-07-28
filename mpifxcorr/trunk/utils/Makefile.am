AM_CPPFLAGS = \
	-I$(top_builddir)/src \
	-I$(top_srcdir)/src

bin_PROGRAMS = checkmpifxcorr dedisperse_difx mpispeed

dist_bin_SCRIPTS = \
	genmachines.py \
	jobdisks \
	joblist \
	jobstatus \
	startdifx

checkmpifxcorr_SOURCES = \
	checkmpifxcorr.cpp

dedisperse_difx_SOURCES = \
	dedisperse_difx.cpp

mpispeed_SOURCES = \
	mpispeed.cpp

checkmpifxcorr_LDADD = ../src/libmpifxcorr.a

dedisperse_difx_LDADD = ../src/libmpifxcorr.a
