.PHONY : all clean

TARGETS = libvlba_utils.a test_vlba_stream

vlba_stream_objs = vlba_stream.o vlba_unpack.o vlba_stream_memory.o vlba_stream_file.o

all   : $(TARGETS)

libvlba_utils.a: $(vlba_stream_objs)

test_vlba_stream: libvlba_utils.a test_vlba_stream.o
	gcc -o test_vlba_stream test_vlba_stream.o libvlba_utils.a

clean : 
	rm -f *.o $(TARGETS)

CFLAGS = -O2 -Wall -fPIC 

libvlba_utils.a: $(vlba_stream_objs)
	ar r libvlba_utils.a $(vlba_stream_objs)

vlba_stream.o: vlba_stream.h
vlba_unpack.o: vlba_stream.h
vlba_stream_memory.o: vlba_stream.h
vlba_stream_file.o: vlba_stream.h
