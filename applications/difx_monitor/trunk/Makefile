.PHONY : all clean

TARGETS := difx_monitor monitor_server mon_sample 

all   : $(TARGETS)

clean : $(TARGETS)
	rm -f *.o $^

CXX = $(MPICXX) -g 
#CPPFLAGS = -O2 -D__PWD=\"$$PWD\"
CPPFLAGS =  -D__PWD=\"$$PWD\"
F77 = g77

INCS = -I$(DIFXROOT)/include/ -I$(IPPROOT)/include 
LIBS =  -L$(DIFXROOT)/lib/ -L$(IPPROOT)/sharedlib -lmpifxcorr -lmark5access -ldifxmessage -lippvm -lguide -lipps -lippcore -lcpgplot -lpgplot -lX11 -lg2c -lpng 

difx_monitor : difx_monitor.cpp
	$(CXX) $(CPPFLAGS) $(INCS) -o $@ $^ $(LIBS)

monitor_server : monitor_server.cpp monserver.c monserver.h
	$(CXX) $(CPPFLAGS) $(INCS) -o $@ $^ $(LIBS)


mon_sample: mon_sample.c monserver.c monserver.h
	$(CXX) $(CPPFLAGS) $(INCS) -o $@  $^ $(LIBS)

mon3d: mon3d.c monserver.c
	$(CXX) $(CPPFLAGS) $(INCS) -o $@  $^ $(LIBS)
