# $Id: //streamstor/example/example.mk#9 $
SSLIBNAME = libssapi.a

all:example

CC = g++
AR = ar

#Set STREAMSTOR to the top level directory of the Streamstor files
STREAMSTOR = /usr/local/streamstor/linux

# Where to install
INSTALLPATH = /usr/local/bin
INSTALLDIR = streamstor

INCLUDE = -I. -I$(STREAMSTOR)/include
SSLIBDIR = $(STREAMSTOR)/lib/gcc_v4

# The -L path should point to where you have installed libssapi.
# WDVER should be the version of WinDriver you are using.
WDVER=1031
LIBS = -L$(SSLIBDIR) -lssapi -lpthread -lrt -lwdapi$(WDVER)

SRCS = example.c
OBJS = example.o

CFLAGS = -g -Wall $(INCLUDE) -DLINUX

example: $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

clean:
	rm -f example.o	

