dist_bin_SCRIPTS = \
	addMark5b.pl \
	addVDIF.pl \
	day2mjd \
	difxcopy \
	dir2filelist.pl \
	filelist2human.pl \
	filterDifx2Fits.py \
	fslog2difx.pl \
	geteop.pl \
	mjd2day \
	pcList.pl \
	sec2time.pl \
	startcorr.pl \
	summarizeDifxlogs.py \
	summarizePolconvertLogs.py \
	time.pl \
	time2sec.pl \
	update_eop \
	vdifheader.pl \
	vex2setup.pl \
	mk5bheader.pl 

bin_PROGRAMS = m5fb_makeheader

m5fb_makeheader_SOURCES = \
	m5fb_makeheader.c

install-exec-hook:
	mv $(DESTDIR)$(bindir)/filterDifx2Fits.py $(DESTDIR)$(bindir)/filterDifx2Fits
