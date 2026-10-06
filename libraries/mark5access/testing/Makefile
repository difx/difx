# Correctness tests and benchmarks for the mark5access VDIF unpackers.
#
# Links against the libtool-built static library in ../mark5access, which is
# rebuilt first so that edits to format_vdif.c are always picked up.
#
#   make            build test and benchmark programs
#   make test       run the correctness tests
#   make bench      run the benchmark over all modes
#
# Override e.g. CC, CFLAGS, M5LIB or EXTRALIBS on the command line as needed.

M5TOP     = ..
M5LIBDIR  = $(M5TOP)/mark5access
M5LIB     = $(M5LIBDIR)/.libs/libmark5access.a

CC       ?= cc
CFLAGS   ?= -O2 -g
CFLAGS   += -Wall -I$(M5TOP)
# libmark5access may be linked with codifio; take the flags configure found
EXTRALIBS ?= $(shell sed -n 's/^CODIFIO_LIBS = //p' $(M5LIBDIR)/Makefile 2>/dev/null)
LDLIBS    = $(M5LIB) $(EXTRALIBS) -lm

PROGS = test_vdif_unpack bench_vdif_unpack

all: $(PROGS)

$(M5LIB): FORCE
	$(MAKE) -C $(M5LIBDIR)

vdif_common.o: vdif_common.c vdif_common.h

test_vdif_unpack: test_vdif_unpack.c vdif_common.o vdif_common.h $(M5LIB)
	$(CC) $(CFLAGS) -o $@ test_vdif_unpack.c vdif_common.o $(LDLIBS)

bench_vdif_unpack: bench_vdif_unpack.c vdif_common.o vdif_common.h $(M5LIB)
	$(CC) $(CFLAGS) -o $@ bench_vdif_unpack.c vdif_common.o $(LDLIBS)

test: test_vdif_unpack
	./test_vdif_unpack
	SYMMETRIC_8_BIT_VDIF=1 ./test_vdif_unpack -m '*-8bit-*'

bench: bench_vdif_unpack
	./bench_vdif_unpack

clean:
	rm -f $(PROGS) *.o
	rm -rf *.dSYM

FORCE:

.PHONY: all test bench clean FORCE
