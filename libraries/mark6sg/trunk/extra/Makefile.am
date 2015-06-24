
bin_PROGRAMS = m6sg_blocknrs m6sg_gather mk6copy fuseMk6
bin_SCRIPTS = m6sg_mount
dist_bin_SCRIPTS = m6sg_mount

m6sg_blocknrs_SOURCES = m6sg_blocknrs.c

m6sg_gather_CFLAGS = -I$(top_srcdir)/mark6sg -I$(srcdir)/.. -Wall
m6sg_gather_LDADD = $(top_builddir)/mark6sg/libmark6sg.la -lm
m6sg_gather_SOURCES = m6sg_gather.c

# fuseMk6_LDADD = $(top_builddir)/mark6sg/libmark6sg.la -lm -lfuse 
fuseMk6_LDADD = $(top_builddir)/mark6sg/.libs/libmark6sg.a -lm -lfuse 
fuseMk6_CFLAGS = -I$(top_srcdir)/mark6sg -I$(srcdir)/.. -Wall -D_FILE_OFFSET_BITS=64 -O3 -g

# mk6copy_LDADD = $(top_builddir)/mark6sg/libmark6sg.la -lm -lrt
mk6copy_LDADD = $(top_builddir)/mark6sg/.libs/libmark6sg.a -lm -lrt
mk6copy_CFLAGS = -I$(top_srcdir)/mark6sg -I$(srcdir)/.. -Wall -D_FILE_OFFSET_BITS=64 -O3 -g

