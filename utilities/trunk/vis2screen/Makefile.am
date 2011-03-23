bin_PROGRAMS = printDiFX
dist_bin_SCRIPTS = diffDiFX.py plotDiFX.py parseDiFX.py fringeFindDiFX.py plotDynamicSpectrum.py snipDiFX.py

AM_CXXFLAGS = $(DIFXMESSAGE_CFLAGS) $(FXCORR_CFLAGS) $(IPP_CFLAGS) 
printDiFX_SOURCES = printDiFX.cpp
printDiFX_LDADD = $(DIFXMESSAGE_LIBS) $(FXCORR_LIBS) $(IPP_LIBS)
