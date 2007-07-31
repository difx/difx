.PHONY : all clean

TARGETS := difx_monitor

all   : $(TARGETS)

clean : $(TARGETS)
	rm -f *.o $^

CC  = mpicc -g
CXX = mpicxx -g 
CPPFLAGS = -O2 -D__PWD=\"$$PWD\"
F77 = g77

IPPROOT = /usr/local/intel/ipp41/ia32_itanium/

INCS = -I$(CORR_ROOT)/mpifxcorr/src/ -I$(IPPROOT)/include -I$(CORR_ROOT)/vlba_utils/ -I/usr/local/pgplot
LIBS = -L$(CORR_ROOT)/mpifxcorr/src/ -L$(IPPROOT)/sharedlib -L$(IPPROOT)/sharedlib/linux32 -L$(CORR_ROOT)/vlba_utils/ -lmpifxcorr -lvlba_utils -lippvm -lguide -lipps -lippcore -L/usr/local/pgplot -lcpgplot -lpgplot -L/usr/X11R6/lib -lX11 -lg2c

difx_monitor : difx_monitor.cpp
	$(CXX) $(CPPFLAGS) $(INCS) -o $@ $^ $(LIBS)


