bin_PROGRAMS = filterbank2screen filterbankcount

AM_CXXFLAGS = $(FXCORR_CFLAGS) $(DIFXMESSAGE_CFLAGS) $(IPP_CFLAGS)

filterbank2screen_SOURCES = filterbank2screen.cpp
filterbank2screen_LDADD = $(FXCORR_LIBS) $(DIFXMESSAGE_LIBS) $(IPP_LIBS) -lcpgplot -lpgplot -lX11

filterbankcount_SOURCES = filterbankcount.cpp
filterbankcount_LDADD = $(DIFXMESSAGE_LIBS) $(FXCORR_LIBS) $(IPP_LIBS)
