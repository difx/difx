#
# $Id: Makefile.am 4585 2026-05-22 17:14:56Z gbc $
#

#AUTOMAKE_OPTIONS = serial-tests 

SUBDIRS =

if HAVE_PYTHON
pkgpython_PYTHON = hopstest.py
check_SCRIPTS =  hopstestsuite.py
endif

# 2611 is the oldest fringe that I have found that is still viable
# it was correlated several times and this was the last of that series
# which was tuned up so that no control file is needed.  The observation
# was made in 1993 and last correlated in 1999.
EXP_2611 = testdata/2611/062-094600/3C345.nrxoiv \
		testdata/2611/062-094600/B..nrxoiv \
		testdata/2611/062-094600/BY..nrxoiv \
		testdata/2611/062-094600/Y..nrxoiv

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

# this is an old S/X RD experiment from 2004 it clearly
# had some issues to be investigated, but it is not clear
# what the resolution was.
EXP_3064 = testdata/3064/113-0256/4C39_25.qxajks \
		testdata/3064/113-0256/N..qxajks \
		testdata/3064/113-0256/NV..qxajks \
		testdata/3064/113-0256/NV.S.9.qxajks \
		testdata/3064/113-0256/NV.X.10.qxajks \
		testdata/3064/113-0256/V..qxajks

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

# 3769 is a several fringes from the ALMA Band7 commissioning test
# this data is publicly available on the ALMA archive.  More scans
# could be added to shake out issues on crappy scans.
EXP_3769 =  testdata/3769/109-0356/3C279.1V53AD \
			testdata/3769/109-0356/A..1V53AD \
			testdata/3769/109-0356/AP.B.37.1V53AD \
			testdata/3769/109-0356/AP.B.38.1V53AD \
			testdata/3769/109-0356/AP.B.39.1V53AD \
			testdata/3769/109-0356/AP.B.40.1V53AD \
			testdata/3769/109-0356/P..1V53AD \
			testdata/3769/109-0506/3C279.1V53B1 \
			testdata/3769/109-0506/A..1V53B1 \
			testdata/3769/109-0506/AJ.B.1.1V53B1 \
			testdata/3769/109-0506/AJ.B.2.1V53B1 \
			testdata/3769/109-0506/AJ.B.3.1V53B1 \
			testdata/3769/109-0506/AJ.B.4.1V53B1 \
			testdata/3769/109-0506/AS.B.45.1V53B1 \
			testdata/3769/109-0506/AS.B.46.1V53B1 \
			testdata/3769/109-0506/AS.B.47.1V53B1 \
			testdata/3769/109-0506/AS.B.48.1V53B1 \
			testdata/3769/109-0506/J..1V53B1 \
			testdata/3769/109-0506/S..1V53B1

AVERAGEFIX = testdata/average \
	     testdata/average/080-low-2-subset.1sec \
	     testdata/average/080-low-2-subset.averaged

test_gs_device = nullpage

TESTS_ENVIRONMENT = HOPS3=true \
	srcdir=$(srcdir) bindir=$(bindir) abs_srcdir=$(abs_srcdir) \
	GS_DEVICE=$(test_gs_device) PGPLOT_DIR=$(PGPLOT_DIR) \
	PS2PDF=@PS2PDF@ fourfit=fourfit

XFAIL_TESTS =
if HAVE_DEVEL
# chk_ff_3571.sh subsumed into chk_min_weight.sh
TESTS = chk_ff_2611.sh chk_ff_2836.sh chk_ff_2843.sh chk_ff_3064.sh \
	chk_ff_3372.sh chk_ff_3413.sh chk_ff_3756.sh \
	chk_baselines.sh chk_alist.sh chk_adump.sh \
	chk_aedit.sh chk_fringex.sh chk_average.sh \
	chk_avefix.sh chk_cofit.sh chk_cohfit.sh chk_cohfit_driver.sh \
	chk_search.sh chk_fourmer.sh chk_frmrsrch.sh chk_hdlinks.sh \
	chk_min_weight.sh chk_passband.sh chk_notches.sh chk_notches_xf.sh \
	chk_ff_3772pb.sh chk_misc_3756.sh chk_misc_3372.sh
# FIXME: gs issue and seg fault race condition on input?
# chk_ff_display.sh
if HAVE_HSEARCH
# the ps2pdf stuff is here as the counts are wrong
# if chk_flagging.sh and chk_ff_dump.sh are not run
TESTS += chk_flagging.sh chk_ff_dump.sh \
	chk_ps2pdf.sh chk_ff_ps2pdf.sh
else !HAVE_HSEARCH
endif !HAVE_HSEARCH
if HAVE_PYTHON
TESTS += run_testsuite.sh
endif
# experiment data for devel
expdata = $(EXP_2611) $(EXP_2836) $(EXP_2843) $(EXP_3064) $(EXP_3365) \
	$(EXP_3372) $(EXP_3413) $(EXP_3571) $(EXP_3562) $(EXP_3756) $(EXP_3727) \
	$(EXP_3769) $(EXP_3772) $(AVERAGEFIX)
else !HAVE_DEVEL
TESTS = chk_ff_2611.sh chk_ff_2843.sh chk_ff_3064.sh chk_baselines.sh \
	chk_alist.sh chk_adump.sh chk_aedit.sh chk_fringex.sh \
	chk_average.sh chk_cohfit_driver.sh \
	chk_search.sh chk_fourmer.sh chk_hdlinks.sh
# experiment data for common
expdata = $(EXP_2611) $(EXP_2843) $(EXP_3064) $(EXP_3365) $(EXP_3769)
endif !HAVE_DEVEL

EXTRA_DIST = $(expdata)

dist_check_SCRIPTS = chk_env.sh $(TESTS)

CLEANFILES = *.ps *.oifhak aedit.in *.out *.alist *.cofit *.coavg cf* *.avg \
	     hopstestsuite.py *.pdf *.awk *.gnu *.cohfit

clean-local:
	rm -rf DD 2843 3365 path datadir pdd* fringes*
	rm -rf cohdrv

#
# eof vim: sts=4
#
