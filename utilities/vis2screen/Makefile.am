bin_PROGRAMS = printDiFX
dist_bin_SCRIPTS = diffDiFX.py diffDiFX2.py checkDiFXViz.py plotDiFX.py \
	plotDiFXPCal.py fringeFindDiFX.py plotDynamicSpectrum.py snipDiFX.py \
	plotResidualSpectrum.py polswapDiFX.py polrelabelDiFX.py stripantennaDiFX.py \
	printDiFX.py printDiFXInput.py printDiFXPCal.py replaceAntennaDiFX.py \
	removeNonzoomAutosDiFX.py removeZerovalAutoDiFX.py plotDiFXLogs.py

AM_CXXFLAGS = $(DIFXMESSAGE_CFLAGS) $(FXCORR_CFLAGS) $(IPP_CFLAGS) 
printDiFX_SOURCES = printDiFX.cpp
printDiFX_LDADD =  $(FXCORR_LIBS) $(DIFXMESSAGE_LIBS) $(IPP_LIBS)
