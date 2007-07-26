.PHONY : all clean

TARGETS := vlba_stream vlba_unpack

all   : $(TARGETS)

clean : $(TARGETS)
	rm -f *.o $^

CC  = gcc
CXX = g++
F77 = g77

INCS = -I.
LIBS = -L.

vlba_stream : vlba_stream.c
	$(CXX) -fPIC -DVLBAFLOAT -c $(INCS) -o $@.o $^ $(LIBS)
	ar r libvlba_utils.a vlba_stream.o

vlba_unpack : vlba_unpack.c
	$(CXX) -fPIC -DVLBAFLOAT -c $(INCS) -o $@.o $^ $(LIBS)
	ar r libvlba_utils.a vlba_unpack.o
