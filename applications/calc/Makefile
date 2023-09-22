.PHONY : all clean

TARGETS := gencalc_delays

all   : $(TARGETS)

clean : $(TARGETS)
	rm -f *.o $^

CC  = gcc
CXX = g++
CPPFLAGS = -O2 -D__PWD=\"$$PWD\" -Wall
F77 = g77

INCS = -Icalc9
LIBS = -Lcalc9 -lcorcalc 

gencalc_delays : gencalc_delays.cpp
	$(MAKE) -C calc9
	$(CXX) $(CPPFLAGS) $(INCS) -o $@ $^ $(LIBS) -lgfortran

