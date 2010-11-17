bin_PROGRAMS = filterbank2screen filterbankcount fb_reorder split_fb_scan

AM_CXXFLAGS = $(FXCORR_CFLAGS) $(DIFXMESSAGE_CFLAGS) $(IPP_CFLAGS) -I$(PGPLOTDIR)

filterbank2screen_SOURCES = filterbank2screen.cpp
filterbank2screen_LDADD = $(FXCORR_LIBS) $(DIFXMESSAGE_LIBS) $(IPP_LIBS) -lcpgplot -lpgplot $(X11LIBS)

filterbankcount_SOURCES = filterbankcount.cpp
filterbankcount_LDADD = $(DIFXMESSAGE_LIBS) $(FXCORR_LIBS) $(IPP_LIBS)

fb_reorder_SOURCES = fb_reorder.cpp readConfig.c difx_fb.h
fb_reorder_LDADD = $(DIFXMESSAGE_LIBS) $(FXCORR_LIBS) $(IPP_LIBS)

split_fb_scan_SOURCES = split_fb_scan.c difx_fb.h
