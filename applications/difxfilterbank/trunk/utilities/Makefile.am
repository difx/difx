bin_PROGRAMS = filterbank2screen

AM_CXXFLAGS = $(DIFXMESSAGE_CFLAGS) $(FXCORR_CFLAGS) $(IPP_CFLAGS)

filterbank2screen_SOURCES = filterbank2screen.cpp
filterbank2screen_LDADD = $(DIFXMESSAGE_LIBS) $(FXCORR_LIBS) $(IPP_LIBS) -lcpgplot -lpgplot -lX11
