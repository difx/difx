.PHONY : all clean

TARGETS := gencalc_delays

all   : $(TARGETS)

clean : $(TARGETS)
	rm -f *.o $^

CC  = gcc
CXX = g++
CPPFLAGS = -O2 -D__PWD=\"$$PWD\"
F77 = g77

INCS = -I.
LIBS = -L.

CALCROOT = /home/vlbi/calc9/

CALCINC = -I$(CALCROOT)/
CALCLIB = -L$(CALCROOT)/ -lcorcalc -lg2c

INCS += $(CALCINC)
LIBS += $(CALCLIB)

gencalc_delays : gencalc_delays.cpp
	$(CXX) $(CPPFLAGS) $(INCS) -o $@ $^ $(LIBS)

