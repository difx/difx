.PHONY : all clean

TARGETS := gencalc_delays

all   : $(TARGETS)

clean : $(TARGETS)
	rm -f *.o $^

CC  = gcc
CXX = g++
CPPFLAGS = -O2 -D__PWD=\"$$PWD\"
F77 = g77

INCS = -I$(DIFXROOT)/include/ -Icalc9
LIBS = -Lcalc9 -L$(G2CDIR) -lcorcalc -lg2c

gencalc_delays : gencalc_delays.cpp
	$(MAKE) -C calc9
	$(CXX) $(CPPFLAGS) $(INCS) -o $@ $^ $(LIBS)

