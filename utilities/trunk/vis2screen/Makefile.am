bin_PROGRAMS = printDiFX
dist_bin_SCRIPTS = diffDiFX.py plotDiFX.py plotDiFXPCal.py parseDiFX.py fringeFindDiFX.py plotDynamicSpectrum.py snipDiFX.py polswapDiFX.py stripantennaDiFX.py printDiFX.py

AM_CXXFLAGS = $(DIFXMESSAGE_CFLAGS) $(FXCORR_CFLAGS) $(IPP_CFLAGS) 
printDiFX_SOURCES = printDiFX.cpp
printDiFX_LDADD =  $(FXCORR_LIBS) $(DIFXMESSAGE_LIBS) $(IPP_LIBS)
