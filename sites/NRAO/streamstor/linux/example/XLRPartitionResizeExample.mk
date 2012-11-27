# $Id: //streamstor/example/XLRPartitionResizeExample.mk#6 $

all:XLRPartitionResizeExample

CC = g++

#Set STREAMSTOR to the top level directory of the Streamstor files
STREAMSTOR = /usr/local/streamstor/linux

# Where to install
INSTALLPATH = /usr/local/bin
INSTALLDIR = streamstor

INCLUDE = -I. -I$(STREAMSTOR)/include
SSLIBDIR = $(STREAMSTOR)/lib/gcc_v4

# The -L path should point to where you have installed libssapi.
# WDVER should be the version of WinDriver you are using.
#
WDVER=1031
SSLIBDIR = $(STREAMSTOR)/lib/gcc_v4
LIBS = -L$(SSLIBDIR) -lssapi -lpthread -lrt -lwdapi$(WDVER)

SRCS = XLRPartitionResizeExample.c
OBJS = XLRPartitionResizeExample.o

CFLAGS = -g -Wall $(INCLUDE) -DLINUX

XLRPartitionResizeExample: $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

clean:
	rm -f XLRPartitionResizeExample.o	
