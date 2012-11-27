# $Id: //streamstor/example/UserDirExample.mk#9 $
all:UserDirExample

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

SRCS = UserDirExample.c
OBJS = UserDirExample.o

CFLAGS = -g -Wall $(INCLUDE) -DLINUX

UserDirExample: $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

clean:
	rm -f UserDirExample.o	
