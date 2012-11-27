# $Id: //streamstor/example/fpdpExample.mk#8 $

SSLIBNAME = libssapi.a

all:fpdpExample

CC = g++

#Set STREAMSTOR to the top level directory of the Streamstor files
STREAMSTOR = /usr/local/streamstor/linux

INCLUDE = -I. -I$(STREAMSTOR)/include

# SSLIBDIR is the PATH of the SDK library you want to link with.
# WDVER should be the version of WinDriver you are using.
WDVER=1031
SSLIBDIR = $(STREAMSTOR)/lib/gcc_v4
LIBS = -L$(SSLIBDIR) -lssapi -lpthread -lrt -lwdapi$(WDVER)

SRCS = fpdpExample.c
OBJS = fpdpExample.o

CFLAGS = -g -Wall $(INCLUDE) -DLINUX

fpdpExample: $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

clean:
	rm -f fpdpExample.o	
