
bin_PROGRAMS = m6sg_blocknrs m6sg_gather
bin_SCRIPTS = m6sg_mount

m6sg_blocknrs_SOURCES = m6sg_blocknrs.c

m6sg_gather_CFLAGS = -I$(top_srcdir)/mark6sg -I$(srcdir)/.. -Wall
m6sg_gather_LDADD = $(top_builddir)/mark6sg/libmark6sg.la -lm
m6sg_gather_SOURCES = m6sg_gather.c
