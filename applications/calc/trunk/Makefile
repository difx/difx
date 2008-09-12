.PHONY : all clean

TARGETS := gencalc_delays

all   : $(TARGETS)

clean : $(TARGETS)
	rm -f *.o $^

CC  = gcc
CXX = g++
CPPFLAGS = -O2 -D__PWD=\"$$PWD\"
F77 = g77

INCS = -I$(DIFXROOT)/include/
LIBS = -L$(DIFXROOT)/lib/ -L$(G2CDIR) -lcorcalc -lg2c

gencalc_delays : gencalc_delays.cpp
	$(CXX) $(CPPFLAGS) $(INCS) -o $@ $^ $(LIBS)

