INCLUDES = -I$(top_srcdir)/mark6sg -I$(top_srcdir)
LDADD = $(top_builddir)/mark6sg/libmark6sg.la -lm

check_PROGRAMS = m6sg_test1 m6sg_test2

m6sg_test1_SOURCES = m6sg_test1.c
m6sg_test1_CFLAGS = -Wall
m6sg_test1_LDADD = $(top_builddir)/mark6sg/libmark6sg.la -lm
m6sg_test1_LDFLAGS = -static

m6sg_test2_SOURCES = m6sg_test2.c
m6sg_test2_CFLAGS = -Wall

