
bin_PROGRAMS = m5tone

m5tone_SOURCES = m5tone.c

m5tone_CFLAGS =  -I/cluster/pgplot -L$(libdir) -I$(includedir)
m5tone_LDADD = -lmark5access -lm -lfftw3 -lgfortran -lX11 -lpng /cluster/pgplot/libcpgplot.a /cluster/pgplot/libpgplot.a
