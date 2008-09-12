.PHONY : all clean

TARGETS := difx_monitor

all   : $(TARGETS)

clean : $(TARGETS)
	rm -f *.o $^

CXX = $(MPICXX) -g 
CPPFLAGS = -O2 -D__PWD=\"$$PWD\"
F77 = g77

INCS = -I$(DIFXROOT)/include/ -I$(IPPROOT)/include -I$(PGPLOTDIR)
LIBS = -L$(G2CDIR) -L$(DIFXROOT)/lib/ -L$(IPPROOT)/sharedlib -lmpifxcorr -lippvmem64t -lguide -lippsem64t -lippcoreem64t -L$(PGPLOTDIR) -lcpgplot -lpgplot -lX11 -lg2c -lpng -lvlba_utils

difx_monitor : difx_monitor.cpp
	$(CXX) $(CPPFLAGS) $(INCS) -o $@ $^ $(LIBS)


