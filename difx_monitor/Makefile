.PHONY : all clean

TARGETS := difx_monitor

all   : $(TARGETS)

clean : $(TARGETS)
	rm -f *.o $^

CC  = mpicc -g
CXX = mpicxx -g 
CPPFLAGS = -O2 -D__PWD=\"$$PWD\"
F77 = g77

IPPROOT = /opt/intel/ipp/5.2/ia32

CORR_ROOT=..

INCS = -I$(CORR_ROOT)/mpifxcorr/branches/mark5-evlbi/src/ -I$(IPPROOT)/include -I$(CORR_ROOT)/common/mark5access/trunk -I/usr/local/pgplot
LIBS = -L$(CORR_ROOT)/mpifxcorr/branches/mark5-evlbi/src -L$(IPPROOT)/sharedlib -L$(IPPROOT)/sharedlib/linux32 -L$(CORR_ROOT)/lib -lmark5access -lmpifxcorr -lippvm -lguide -lipps -lippcore -L/usr/local/pgplot -lcpgplot -lpgplot -L/usr/X11R6/lib -lX11 -lg2c -lpng

difx_monitor : difx_monitor.cpp
	$(CXX) $(CPPFLAGS) $(INCS) -o $@ $^ $(LIBS)


