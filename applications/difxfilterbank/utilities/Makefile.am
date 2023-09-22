if WITH_PLPLOT
plplotprograms = \
	filterbank2screen

filterbank2screen_SOURCES = filterbank2screen.cpp
else
plplotprograms = 
endif

bin_PROGRAMS = \
	filterbankcount \
	fb_reorder \
	split_fb_scan \
	printfilterbank \
	difx2fb \
	$(plplotprograms)

filterbankcount_SOURCES = filterbankcount.cpp

printfilterbank_SOURCES = printfilterbank.cpp

fb_reorder_SOURCES = fb_reorder.cpp readConfig.c difx_fb.h

difx2fb_SOURCES = difx2fb.cpp

split_fb_scan_SOURCES = split_fb_scan.c difx_fb.h
