#
# $Id: Makefile.am 1527 2017-01-19 13:33:28Z gbc $
#

SUBDIRS = .

vhelpdir = $(pkgdatadir)/vhelp

solid_doc = adump.doc aedit.doc alist.doc \
	    fourfit.doc fourmer.doc \
	    fplot.doc fringex.doc general.doc \
	    libafio.doc libdfio.doc \
	    libutil.doc \
	    scripts.doc vhelp.doc 
devel_doc = calamp.doc coterp.doc linkdata.doc pratio.doc \
	    average.doc bispec.doc cofit.doc fearfit.doc hfold.doc \
	    mmreduce.doc search.doc
quiet_doc = template.doc ff_control.doc ff_output.doc \
	    ff_search.doc ff_vsfrnge.doc

if HAVE_DEVEL
vhelp_DATA = $(solid_doc) $(devel_doc)
EXTRA_DIST = $(solid_doc) $(devel_doc) $(quiet_doc)
else
vhelp_DATA = $(solid_doc)
EXTRA_DIST = $(solid_doc)
endif

# aedit help directory contents
nobase_dist_vhelp_DATA =  aedit/axis aedit/baselines \
			  aedit/batch aedit/bsnrmax aedit/bsnrmin \
			  aedit/ccread aedit/clear aedit/close aedit/device \
			  aedit/edit aedit/exit \
			  aedit/experiment \
			  aedit/formatter \
			  aedit/fplot \
			  aedit/fraction aedit/frequencies aedit/general \
			  aedit/grid aedit/help \
			  aedit/inputs aedit/length \
			  aedit/manual aedit/mode \
			  aedit/nfreq aedit/nobatch \
			  aedit/outversion aedit/parameter aedit/plist \
			  aedit/plot aedit/polarizations aedit/prange \
			  aedit/procrange aedit/psfile aedit/psplot \
			  aedit/pwrite aedit/qcodes aedit/read \
			  aedit/reference aedit/remote aedit/reproc \
			  aedit/run aedit/setyear aedit/snrmax aedit/snrmin \
			  aedit/sort aedit/sources \
			  aedit/stations aedit/summary \
			  aedit/timerange \
			  aedit/twrite aedit/type aedit/unflag aedit/unsort \
			  aedit/write aedit/xscale aedit/yscale aedit/zoom 

#
# eof
#
