# Darwin Intel x86 Makefile for RPFITS (cancer, MacOSX 8.9.1).

# The RPFITS root directory.
RPFITSROOT := ..

# Only used for locating architecture-specific code in ../code/$(RPARCH)
# (../code/darwin_x86 is currently symlink'd to ../code/linux).
RPARCH := darwin_x86

# C compiler flags.  
CC      := gcc
CFLAGS  := -g -O -Wall

# GNU Fortran 77
ifdef USEGFORTRAN
  FC      := gfortran
  FFLAGS  := -g -O -fno-automatic -Wall
  LDFLAGS := -lg2c
else
  FC      := g77
  FFLAGS  := -g -O -fno-automatic -Wall -Wimplicit -Wno-globals
endif

# Use these definitions (in place of those above) to compile with gfortran.

RANLIB  := /usr/bin/ranlib

# Where to install stuff.
PREFIX := /usr/local

#-----------------------------------------------------------------------------
# Include generic RPFITS makefile.
-include $(RPFITSROOT)/GNUmakefile
