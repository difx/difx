bin_PROGRAMS = printDiFX RPFitsReader

AM_CXXFLAGS = $(DIFXMESSAGE_CFLAGS) $(FXCORR_CFLAGS) $(IPP_CFLAGS) 
printDiFX_SOURCES = printDiFX.cpp
printDiFX_LDADD = $(DIFXMESSAGE_LIBS) $(FXCORR_LIBS) $(IPP_LIBS)

RPFitsReader_SOURCES = RPFitsReader.cpp
RPFitsReader_LDADD = -lrpfits -lg2c
