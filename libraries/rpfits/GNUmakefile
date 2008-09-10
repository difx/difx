#-----------------------------------------------------------------------------
# GNUmakefile: Template GNU makefile for compiling the RPFITS library.
#-----------------------------------------------------------------------------
# The GNU makefile in this directory is intended to be included from another
# GNU makefile (usually in a subdirectory) that contains architecture-specific
# variable definitions for:
#
#    RPFITSROOT ...The RPFITS root directory (i.e. this directory).
#    ARCH       ...Architecture name (alpha, darwin, linux, sgi, sun4,
#                  sun4sol, etc.) used only for locating architecture-specific
#                  source code, e.g. in ../code/linux/.
#    FC,CC      ...Names of the Fortran and C compilers should be supplied
#                  by GNU make for your system but can also be redefined if
#                  required.
#    FFLAGS     ...Fortran compiler flags.
#    CFLAGS     ...C compiler flags.
#    RANLIB     ...Command used to "ranlib" object libraries (set to ":" if
#                  ranlib is not needed).
#    LDFLAGS    ...Linker flags.
#    PREFIX     ...Install location, e.g. /usr/local; files are actually
#                  installed in $(PREFIX)/{lib,include,bin}.
#
# If you are trying to install the RPFITS library, please chdir to either the
# linux, sun4sol, or darwin sub-directory and run GNU 'make' from there.  You
# may find it necessary to modify variable definitions (as above) in the
# GNUmakefile there to suit your purposes.
#
# $Id: GNUmakefile,v 1.11 2006/07/17 07:04:38 cal103 Exp $
#-----------------------------------------------------------------------------
ifndef RPFITSROOT
   RPFITSROOT := $(shell pwd)
endif

# Where gmake finds the source code.
COMMSRC := $(RPFITSROOT)/code
ARCHSRC := $(COMMSRC)/$(ARCH)
TESTSRC := $(RPFITSROOT)/test
VPATH   := .:$(ARCHSRC):$(COMMSRC):$(TESTSRC)

# Include paths etc.
INCLUDES := -I. -I$(ARCHSRC) -I$(COMMSRC)
FFLAGS   += $(INCLUDES)
CFLAGS   += -c $(INCLUDES)

# First check out any new sources.
FIRST  := $(shell $(MAKE) -C $(COMMSRC))

# Lists of object library members.
FSUBS  := $(filter-out *.f, \
            $(notdir \
               $(wildcard ./*.f $(COMMSRC)/*.f $(ARCHSRC)/*.f)))
FOBJS  := $(FSUBS:.f=.o)

CSUBS  := $(filter-out rpfhdr.c rpfex.c *.c, \
            $(notdir \
               $(wildcard ./*.c $(COMMSRC)/*.c $(ARCHSRC)/*.c)))
COBJS  := $(CSUBS:.c=.o)

RPFITSLIB  := librpfits.a
MEMBERS := $(patsubst %,$(RPFITSLIB)(%),$(FOBJS) $(COBJS))

ifndef PREFIX
   PREFIX := /usr/local
endif


# Static rules.
.PRECIOUS : $(RPFITSLIB)
.PHONY : all clean lib realclean show test

ifeq "$(ARCH)" ""
all :
	-@ echo ""
	-@ head -29 ./GNUmakefile
	-@ echo ""
else
all : lib rpfhdr rpfex
	-@ $(RM) *.o

lib : $(RPFITSLIB) ;

$(RPFITSLIB) : $(MEMBERS)
	$(RANLIB) $(RPFITSLIB)
	chmod 664 $@

rpfhdr : rpfhdr.c
	$(CC) -c $(CFLAGS) -o $@.o $<
	$(CC) -o $@ $(LDFLAGS) $@.o

rpfex : rpfex.c
	$(CC) -c $(CFLAGS) -o $@.o $<
	$(CC) -o $@ $(LDFLAGS) $@.o

install : all
	-@ mkdir -p $(PREFIX)/lib $(PREFIX)/include $(PREFIX)/bin
	-@ $(RM) $(PREFIX)/lib/librpfits.*
	   cp $(RPFITSLIB) $(PREFIX)/lib
	-@ $(RM) $(PREFIX)/include/rpfits.inc
	   cp $(COMMSRC)/rpfits.inc $(PREFIX)/include
	-@ $(RM) $(PREFIX)/include/RPFITS.h
	   cp $(COMMSRC)/RPFITS.h $(PREFIX)/include
	-@ $(RM) $(PREFIX)/bin/rpfhdr $(PREFIX)/bin/rpfex
	   cp rpfhdr rpfex $(PREFIX)/bin

clean :
	-$(RM) *.o core test.tmp

realclean : clean
	-$(RM) $(RPFITSLIB) tfits

# Compile and execute the test program.
test : tfits
	   $(RM) test.tmp
	-@ echo "Running tfits to write RPFITS data..."
	 @ echo w | ./tfits
	-@ echo "Running tfits to read RPFITS data..."
	 @ echo r | ./tfits

tfits : tfits.o $(RPFITSLIB)
	$(FC) -o $@ $(LDFLAGS) $< -L. -lrpfits
	$(RM) $<

# Include file dependencies.
$(RPFITSLIB)(rpfitsin.o) : rpfits.inc
$(RPFITSLIB)(rpfitsout.o) : rpfits.inc
$(RPFITSLIB)(rpfits_tables.o) : rpfits.inc

show :
	-@echo "RPFITSROOT=$(RPFITSROOT)"
	-@echo "ARCH    =$(ARCH)"
	-@echo ""
	-@echo "RPFITSLIB=$(RPFITSLIB)"
	-@echo ""
	-@echo "COMMSRC =$(COMMSRC)"
	-@echo "ARCHSRC =$(ARCHSRC)"
	-@echo "INCLUDES=$(INCLUDES)"
	-@echo ""
	-@echo "FC      =$(FC)"
	-@echo "FFLAGS  =$(FFLAGS)"
	-@echo "FSUBS   =$(FSUBS)"
	-@echo "FOBJS   =$(FOBJS)"
	-@echo ""
	-@echo "CC      =$(CC)"
	-@echo "CFLAGS  =$(CFLAGS)"
	-@echo "CSUBS   =$(CSUBS)"
	-@echo "COBJS   =$(COBJS)"
	-@echo ""
	-@echo "MEMBERS =$(MEMBERS)"
endif
