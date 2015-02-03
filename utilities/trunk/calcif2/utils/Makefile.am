bin_PROGRAMS = calc2skd

dist_bin_SCRIPTS = \
	jobdisks \
	genmachines.py \
	joblist \
	jobstatus \
	listmodules \
	listcpus \
	startdifx \
	vlog

calc2skd_SOURCES = \
	calc2skd.c

install-exec-hook:
	mv $(DESTDIR)$(bindir)/genmachines.py $(DESTDIR)$(bindir)/genmachines
