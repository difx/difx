# $Id: //streamstor/example/XLRGetFIFOLengthExample.mk#8 $

all:XLRGetFIFOLengthExample

CC = g++

#Set STREAMSTOR to the top level directory of the Streamstor files
STREAMSTOR = /usr/local/streamstor/linux

# Where to install
INSTALLPATH = /usr/local/bin
INSTALLDIR = streamstor

INCLUDE = -I. -I$(STREAMSTOR)/include

#
# SSLIBDIR should point to the SDK library you want to link with.
# WDVER should be the version of WinDriver you are using.
#
WDVER=1031
SSLIBDIR = $(STREAMSTOR)/lib/gcc_v4
LIBS = -L$(SSLIBDIR) -lssapi -lpthread -lrt -lwdapi$(WDVER)

SRCS = XLRGetFIFOLengthExample.c
OBJS = XLRGetFIFOLengthExample.o

CFLAGS = -g -Wall $(INCLUDE) -DLINUX

XLRGetFIFOLengthExample: $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

clean:
	rm -f XLRGetFIFOLengthExample.o	
