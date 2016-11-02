dist_bin_SCRIPTS = dir2filelist.pl geteop.pl pcList.pl fslog2difx.pl sec2time.pl difxcopy startcorr.pl time.pl time2sec.pl update_eop vex2setup.pl filterDifx2Fits.py vdifheader.pl addMark5b.pl

bin_PROGRAMS = m5fb_makeheader

m5fb_makeheader_SOURCES = \
	m5fb_makeheader.c

install-exec-hook:
	mv $(DESTDIR)$(bindir)/filterDifx2Fits.py $(DESTDIR)$(bindir)/filterDifx2Fits
