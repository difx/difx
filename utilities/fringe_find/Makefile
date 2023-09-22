.PHONY : all clean

TARGETS := fringe_find

all   : $(TARGETS)

clean : $(TARGETS)
	rm -f *.o $^

CC  = gcc
CXX = c++
CPPFLAGS = -O2 -D__PWD=\"$$PWD\"
F77 = g77

MPIFXROOT = /home/ssi/adeller/correlator/mpifxcorr/
IPPROOT = /opt/intel
VLBAUTILSROOT = /home/ssi/adeller/correlator/vlba_utils/

IPPINC = -I$(IPPROOT)/ipp/include
IPPLIB = -Wl,-rpath,${IPPROOT}/ipp/lib, -L$(IPPROOT)/ipp/lib -lippvm -lipps -lippcore

INCS = -I.
LIBS = -L.

INCS += $(IPPINC)
LIBS += $(IPPLIB)

fringe_plotDiFX : fringe_plotDiFX.cpp
	$(CXX) $(CPPFLAGS) $(INCS) -I$(MPIFXROOT)/src/ -I$(VLBAUTILSROOT) -o $@ $^ $(LIBS) -L$(MPIFXROOT)/src/ -L$(VLBAUTILSROOT) -lmpifxcorr -lvlba_utils

fringe_find : fringe_find.cpp
	$(CXX) $(CPPFLAGS) $(INCS) -o $@ $^ $(LIBS)
