DIST_EXTRA = m6sg_blocknrs.c mk6copy.c m6sg_gather.c fuseMk6.c
bin_PROGRAMS = m6sg_blocknrs m6sg_gather mk6copy

if USE_FUSE
bin_PROGRAMS += fuseMk6 fuseDBBC3
endif

#dist_bin_SCRIPTS = m6sg_mount

bin_SCRIPTS = m6sg_mount

m6sg_gather_CFLAGS = -I$(top_srcdir)/mark6sg -I$(top_srcdir) -Wall
m6sg_gather_LDADD = $(top_builddir)/mark6sg/libmark6sg.la -lm
m6sg_gather_SOURCES = m6sg_gather.c

fuseMk6_LDADD = $(top_builddir)/mark6sg/libmark6sg.la -lm ${FUSE_LIBS}
fuseMk6_LDFLAGS = -static
fuseMk6_CFLAGS = -I$(top_srcdir)/mark6sg -I$(top_srcdir) -Wall -D_FILE_OFFSET_BITS=64 -O3 -g
fuseMk6_SOURCES = fuseMk6.c

fuseDBBC3_LDADD = $(top_builddir)/mark6sg/libmark6sg.la -lm ${FUSE_LIBS}
fuseDBBC3_LDFLAGS = -static
fuseDBBC3_CFLAGS = -I$(top_srcdir)/mark6sg -I$(top_srcdir) -Wall -D_FILE_OFFSET_BITS=64 -O3 -g --std=gnu99
fuseDBBC3_SOURCES = fuseDBBC3.c

mk6copy_LDADD = $(top_builddir)/mark6sg/libmark6sg.la -lm -lrt
mk6copy_LDFLAGS = -static
mk6copy_CFLAGS = -I$(top_srcdir)/mark6sg -I$(top_srcdir) -Wall -D_FILE_OFFSET_BITS=64 -O3 -g
mk6copy_SOURCES = mk6copy.c

m6sg_blocknrs_SOURCES = m6sg_blocknrs.c
