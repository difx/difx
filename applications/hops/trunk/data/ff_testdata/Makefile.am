#
# $Id: Makefile.am 3345 2021-09-09 20:56:07Z gbc $
#

#AUTOMAKE_OPTIONS = serial-tests 

SUBDIRS =

if HAVE_PYTHON
pkgpython_PYTHON = hopstest.py
check_SCRIPTS =  hopstestsuite.py
endif

# 2836 is Roger's canonical experiment, but it's only two stations
# target_correlator = Haystack;  Mk4/hdw.
EXP_2836 =  testdata/2836/cf2836 \
	    testdata/2836/scan001/2145+067.olomfh \
	    testdata/2836/scan001/AA..olomfh \
	    testdata/2836/scan001/AE..olomfh \
	    testdata/2836/scan001/A..olomfh \
	    testdata/2836/scan001/E..olomfh \
	    testdata/2836/scan001/AE.X.13.olomfh

# 2843 is another scan with 3 stations
# target_correlator = Haystack; Mk4/hdw.
EXP_2843 =  testdata/2843/321-1701_0552+398/0552+398.oifhak \
	    testdata/2843/321-1701_0552+398/AI..oifhak \
	    testdata/2843/321-1701_0552+398/A..oifhak \
	    testdata/2843/321-1701_0552+398/AT..oifhak \
	    testdata/2843/321-1701_0552+398/I..oifhak \
	    testdata/2843/321-1701_0552+398/IT..oifhak \
	    testdata/2843/321-1701_0552+398/T..oifhak

# 3365 is a scan suitable for fourmering
# target_correlator = not specified  Mk4/hdw.
EXP_3365 =  testdata/3365/094-0644_HIGH/3C273.vmudbk \
	    testdata/3365/094-0644_HIGH/OP..vmudbk \
	    testdata/3365/094-0644_HIGH/O..vmudbk \
	    testdata/3365/094-0644_HIGH/P..vmudbk \
	    testdata/3365/094-0644_HIGH/SO..vmudbk \
	    testdata/3365/094-0644_HIGH/SP..vmudbk \
	    testdata/3365/094-0644_HIGH/S..vmudbk \
	    testdata/3365/094-0644_LOW/3C273.vlncsf \
	    testdata/3365/094-0644_LOW/OP..vlncsf \
	    testdata/3365/094-0644_LOW/O..vlncsf \
	    testdata/3365/094-0644_LOW/P..vlncsf \
	    testdata/3365/094-0644_LOW/SO..vlncsf \
	    testdata/3365/094-0644_LOW/SP..vlncsf \
	    testdata/3365/094-0644_LOW/S..vlncsf

# 3372 is a scan with the legacy geodetic X band setup
# target_correlator = Haystack; 0529+483.vqbvvf  Mk4/hdw
# target_correlator = difx;     0529+483.vtqbsq  Mk4/DiFX
EXP_3372 =  testdata/3372/cf3372 \
	    testdata/3372/193-1757/0529+483.vtqbsq \
	    testdata/3372/193-1757/TV..vtqbsq \
	    testdata/3372/193-1757/T..vtqbsq \
	    testdata/3372/193-1757/V..vtqbsq

# 3413 is a broadband experiment
# target_correlator = difx;  Mk4/DiFX
EXP_3413 =  testdata/3413/cf3413 \
	    testdata/3413/278-1758/0552+398.wmtukg \
	    testdata/3413/278-1758/GE..wmtukg \
	    testdata/3413/278-1758/G..wmtukg \
	    testdata/3413/278-1758/E..wmtukg

# 3571 is another broadband experiment (single baseline)
# target_correlator = difx;  Mk4/DiFX
EXP_3571 =  testdata/3571/cf3571 \
	    testdata/3571/cf3571_244-1249 \
            testdata/3571/244-1717/0727-115.zbgwce \
	    testdata/3571/244-1717/E..zbgwce \
	    testdata/3571/244-1717/GE..zbgwce \
	    testdata/3571/244-1717/G..zbgwce

# 3562 is a broadband experiment with three baselines
# target_correlator = difx;  Mk4/DiFX
EXP_3562 =  testdata/3562/cf3562_140-1817 \
	    testdata/3562/141-0002/0727-115.yxhoyl \
	    testdata/3562/141-0002/GH..yxhoyl \
	    testdata/3562/141-0002/EE..yxhoyl \
	    testdata/3562/141-0002/G..yxhoyl \
	    testdata/3562/141-0002/E..yxhoyl \
	    testdata/3562/141-0002/HE..yxhoyl \
	    testdata/3562/141-0002/GE..yxhoyl \
	    testdata/3562/141-0002/HH..yxhoyl \
	    testdata/3562/141-0002/GG..yxhoyl \
	    testdata/3562/141-0002/H..yxhoyl \
	    testdata/3562/141-0002/GE.X.1.yxhoyl \
	    testdata/3562/141-0002/HE.X.2.yxhoyl \
	    testdata/3562/141-0002/GH.X.3.yxhoyl \
	    testdata/3562/3562-captured.tsumm \
	    testdata/3562/3562-captured.fsumm

# 3756 is a geodetic experiment with one baseline
# target_correlator = difx;  Mk4/DiFX
EXP_3756 = testdata/3756/cf_3758_GEHIMSTVY_pstokes4 \
	   testdata/3756/328-1800/1803+784.1TDGBD \
	   testdata/3756/328-1800/H..1TDGBD \
	   testdata/3756/328-1800/HT..1TDGBD \
	   testdata/3756/328-1800/T..1TDGBD

# 3727 is recent a snippet of a commissioning test at ALMA
# target_correlator = difx;  Mk4/DiFX
EXP_3727 = testdata/3727/026-1123/J1337-1257.12SVG0 \
	testdata/3727/026-1123/A..12SVG0 \
	testdata/3727/026-1123/AA..12SVG0 \
	testdata/3727/026-1123/D..12SVG0 \
	testdata/3727/026-1123/DA..12SVG0 \
	testdata/3727/026-1123/DD..12SVG0

# 3772 is a spectral line test useful for passband testing
EXP_3772 = testdata/3772/cf3772 \
	testdata/3772/No0001/1055+018.1T8DS3 \
	testdata/3772/No0001/A..1T8DS3 \
	testdata/3772/No0001/AB..1T8DS3 \
	testdata/3772/No0001/B..1T8DS3

AVERAGEFIX = testdata/average \
	     testdata/average/080-low-2-subset.1sec \
	     testdata/average/080-low-2-subset.averaged

test_gs_device = nullpage

TESTS_ENVIRONMENT = \
	srcdir=$(srcdir) bindir=$(bindir) \
	GS_DEVICE=$(test_gs_device) PGPLOT_DIR=$(PGPLOT_DIR) \
	PS2PDF=@PS2PDF@

# FIXME: gs issue
#       chk_ff_display.sh

if HAVE_DEVEL
# chk_ff_3571.sh subsumed into chk_min_weight.sh
TESTS = chk_ff_2836.sh chk_ff_2843.sh chk_ff_3372.sh \
	chk_ff_3413.sh chk_ff_3756.sh \
	chk_baselines.sh chk_alist.sh chk_adump.sh \
	chk_aedit.sh chk_fringex.sh chk_average.sh \
	chk_avefix.sh chk_cofit.sh chk_search.sh \
	chk_fourmer.sh chk_frmrsrch.sh chk_hdlinks.sh \
	chk_min_weight.sh chk_passband.sh \
	chk_notches.sh chk_notches_xf.sh \
	chk_ff_3772pb.sh
if HAVE_HSEARCH
# the ps2pdf stuff is here as the counts are wrong
# if chk_flagging.sh and chk_ff_dump.sh are not run
TESTS += chk_flagging.sh chk_ff_dump.sh \
	chk_ps2pdf.sh chk_ff_ps2pdf.sh
endif
#XFAIL_TESTS = ...
if HAVE_PYTHON
TESTS += run_testsuite.sh
#XFAIL_TESTS += ...
endif
# experiment data for devel
expdata = $(EXP_2836) $(EXP_2843) $(EXP_3365) $(EXP_3372) $(EXP_3413) \
	$(EXP_3571) $(EXP_3562) $(EXP_3756) $(EXP_3727) $(EXP_3772) \
	$(AVERAGEFIX)
else
TESTS = chk_ff_2843.sh chk_baselines.sh \
	chk_alist.sh chk_adump.sh chk_aedit.sh chk_fringex.sh \
	chk_fourmer.sh chk_hdlinks.sh
# experiment data for common
expdata = $(EXP_2843) $(EXP_3365)
endif

EXTRA_DIST = $(expdata)

dist_check_SCRIPTS = chk_env.sh $(TESTS)

CLEANFILES = *.ps *.oifhak aedit.in *.out *.alist *.cofit *.coavg cf* *.avg \
	     hopstestsuite.py *.pdf *.awk *.gnu

clean-local:
	rm -rf DD 2843 3365 path datadir pdd*

#
# eof
#
