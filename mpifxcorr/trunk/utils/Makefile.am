AM_CPPFLAGS = \
	-I$(top_builddir)/src \
	-I$(top_srcdir)/src

bin_PROGRAMS = checkmpifxcorr dedisperse_difx mpispeed

dist_bin_SCRIPTS = \
	genmachines.py \
	jobdisks.py \
	joblist.py \
	jobstatus.py \
	startdifx.py \
	stopmpifxcorr.py\
	calcifMixed.py

checkmpifxcorr_SOURCES = \
	checkmpifxcorr.cpp

dedisperse_difx_SOURCES = \
	dedisperse_difx.cpp

mpispeed_SOURCES = \
	mpispeed.cpp

checkmpifxcorr_LDADD = ../src/libmpifxcorr.a

dedisperse_difx_LDADD = ../src/libmpifxcorr.a

install-exec-hook:
	mv $(DESTDIR)$(bindir)/genmachines.py $(DESTDIR)$(bindir)/genmachines
	mv $(DESTDIR)$(bindir)/calcifMixed.py $(DESTDIR)$(bindir)/calcifMixed
	mv $(DESTDIR)$(bindir)/jobdisks.py $(DESTDIR)$(bindir)/jobdisks
	mv $(DESTDIR)$(bindir)/joblist.py $(DESTDIR)$(bindir)/joblist
	mv $(DESTDIR)$(bindir)/jobstatus.py $(DESTDIR)$(bindir)/jobstatus
	mv $(DESTDIR)$(bindir)/startdifx.py $(DESTDIR)$(bindir)/startdifx
	mv $(DESTDIR)$(bindir)/stopmpifxcorr.py $(DESTDIR)$(bindir)/stopmpifxcorr

