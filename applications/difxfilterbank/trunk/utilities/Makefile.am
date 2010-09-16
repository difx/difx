bin_PROGRAMS = filterbank2screen filterbankcount fb_reorder

AM_CXXFLAGS = $(FXCORR_CFLAGS) $(DIFXMESSAGE_CFLAGS) $(IPP_CFLAGS)

filterbank2screen_SOURCES = filterbank2screen.cpp
filterbank2screen_LDADD = $(FXCORR_LIBS) $(DIFXMESSAGE_LIBS) $(IPP_LIBS) -lcpgplot -lpgplot $(X11LIBS)

filterbankcount_SOURCES = filterbankcount.cpp
filterbankcount_LDADD = $(DIFXMESSAGE_LIBS) $(FXCORR_LIBS) $(IPP_LIBS)

fb_reorder_SOURCES = fb_reorder.cpp readConfig.c
fb_reorder_LDADD = $(DIFXMESSAGE_LIBS) $(FXCORR_LIBS) $(IPP_LIBS)
