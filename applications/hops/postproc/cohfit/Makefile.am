#
# $Id: Makefile.am 4569 2026-05-21 17:12:25Z gbc $
#

SUBDIRS = .

# taken from
# https://sites.astro.caltech.edu/~tjp/pgplot/chapter5.html
#  usegslibs/cpgplot-colors.txt
# is not distributable.
EXTRA_DIST = $(srcdir)/gslcheck/source-info.txt \
	$(srcdir)/gslcheck/gslexpfit.output \
	$(srcdir)/gslcheck/gslexp2p1.output \
	$(srcdir)/gslcheck/gslexp2p0.output \
	$(srcdir)/gslcheck/gslspline2p8.output \
	$(srcdir)/gslcheck/gslspline2p7.output \
	$(srcdir)/gslcheck/exminimizer.output

# one way or another, this gets made
AM_CPPFLAGS = @HOPS_INC@ -DPS2PDF=\"@PS2PDF@\" -DBASH=\"$(BASH)\" \
	-DGNUPLOT=\"$(GNUPLOT)\" -DMONTAGE=\"$(MONTAGE)\"
bin_PROGRAMS = cohfit
check_PROGRAMS = exminimizer
gsltest = exminimizer gslcheck/exminimizer.sh

# gsl/gsl_multifit_nlinear.h appeared in GSL 2.2
# gsl/gsl_multifit_nlin.h we presume, existed prior
if HAVE_GSL115
gslt115 = expfitexam2p1 gslcheck/expfit2p1chk.sh \
	  expfitexam2p0 gslcheck/expfit2p0chk.sh
check_PROGRAMS += expfitexam2p1 expfitexam2p0
endif
if HAVE_GSL_22
gslt2p2 = expfitexample gslcheck/expfitcheck.sh
check_PROGRAMS += expfitexample
endif

# basis spline framework changed 2.7->2.8
if HAVE_GSL_28
gslt2p8 = cbsplinelsqex2p8 gslcheck/cbspline2p8chk.sh
check_PROGRAMS += cbsplinelsqex2p8
AM_CPPFLAGS += -DHAVEGSL2P8=1
usingslib2p8 = usegslibs/min_inv_snr_cbs2p8.c usegslibs/fit_cbs2p8.c
endif
if HAVE_GSL_27
gslt2p7 = cbsplinelsqex2p7 gslcheck/cbspline2p7chk.sh
check_PROGRAMS += cbsplinelsqex2p7
AM_CPPFLAGS += -DHAVEGSL2P7=1
usingslib2p7 = usegslibs/min_inv_snr_cbs2p7.c usegslibs/fit_cbs2p7.c
endif

cbsplinelsqex2p8_SOURCES = gslcheck/cbsplinelsqex2p8.c
cbsplinelsqex2p8_LDADD = @GSL_LIBS@
cbsplinelsqex2p7_SOURCES = gslcheck/cbsplinelsqex2p7.c
cbsplinelsqex2p7_LDADD = @GSL_LIBS@

expfitexam2p0_SOURCES = gslcheck/expfitexam2p0.c
expfitexam2p0_LDADD = @GSL_LIBS@
expfitexam2p1_SOURCES = gslcheck/expfitexam2p1.c
expfitexam2p1_LDADD = @GSL_LIBS@
expfitexample_SOURCES = gslcheck/expfitexample.c
expfitexample_LDADD = @GSL_LIBS@
exminimizer_SOURCES = gslcheck/exminimizer.c
exminimizer_LDADD = @GSL_LIBS@

dist_check_SCRIPTS = gslcheck/expfitcheck.sh \
		     gslcheck/expfit2p0chk.sh \
		     gslcheck/expfit2p1chk.sh \
		     gslcheck/cbspline2p8chk.sh \
		     gslcheck/cbspline2p7chk.sh \
		     gslcheck/exminimizer.sh

TESTS_ENVIRONMENT = srcdir=$(srcdir)
TESTS = $(gsltest) $(gslt115) $(gslt2p2) $(gslt2p7) $(gslt2p8)

# these make no use of numerical recipes -- GOOD
usingslheader = usegslibs/fit_gsl.h usegslibs/exam_gnuplot.h
usingslibrary = \
	usegslibs/fit_ampl.c usegslibs/as_fit_nm_ndx.c \
	usegslibs/fit_amp_ps.c usegslibs/fit_amp_po.c \
	usegslibs/fit_amp_so.c usegslibs/gcy_amp_ps.c \
	usegslibs/gcy_amp_po.c usegslibs/gcy_amp_so.c \
	usegslibs/choose_best_amp_fit.c \
	usegslibs/err_handler.c usegslibs/fit_snr.c \
	usegslibs/fit_msrn.c usegslibs/exam_edits.c \
	usegslibs/exam_file.c usegslibs/exam_gnuplot.c

# these sources manage the data and are the same either way
common_cohfit_code = clear_codata.c fit_codata.c parse_cmdline.c \
	sort_data.c cohfit.c plot_codata.c read_data.c \
	sorter.c write_codata.c normalize_snr.c
# fit_snr is also common, but modified

# now follows a tortured set of conditionals, but basically
# you can only build this beast if you have various libraries
# needed by the tool (and several other HOPS applications) and
# a) you want to build the original code !PURGE_NR
# b) you do not want NR (PURGE_NR true) and you have GSL
# so that you can fit things.
if HAVE_PGPLOT
if HAVE_X11
if HAVE_PNG

if PURGE_NR

if HAVE_GSL115
noinst_HEADERS = cohfit.h $(usingslheader)

cohfit_SOURCES = $(common_cohfit_code) $(usingslibrary) \
	$(usingslib2p8) $(usingslib2p7)

cohfit_CPPLAGS = $(AM_CPPFLAGS) -I$(srcdir)/usegslibs
cohfit_LDADD = @PGP_LIB@ @PNG_LIB@ @MSG_LIB@ \
	@AFIO_LIB@ @UTIL_LIB@ @X11_LIB@ @GSL_LIBS@
else !HAVE_GSL115
cohfit_SOURCES = you_lose.c
endif !HAVE_GSL115

else !PURGE_NR
cohfit_SOURCES = you_lose.c
fi
endif !PURGE_NR
cohfit_DEPENDENCIES = @MSG_DEP@ @AFIO_DEP@ @UTIL_DEP@

else !HAVE_PNG
cohfit_SOURCES = you_lose.c
endif !HAVE_PNG
else !HAVE_X11
cohfit_SOURCES = you_lose.c
endif !HAVE_X11
else !HAVE_PGPLOT
cohfit_SOURCES = you_lose.c
endif !HAVE_PGPLOT

you_lose.c: Makefile.am
	echo '#include <stdio.h>' > $@
	echo 'int main()' >> $@
	echo '{return(puts("Missing PNG, X11, PGPLOT or GSL > 2.7\n"));}' >> $@

CLEANFILES = you_lose.c efe????.* efe????2p?.* cbsdata* cbsgraph*.* 

#
# eof vim:nospell ts=8
#
