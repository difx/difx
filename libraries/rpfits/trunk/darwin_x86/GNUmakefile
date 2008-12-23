# Darwin Intel x86 Makefile for RPFITS (cancer, MacOSX 8.9.1).

# The RPFITS root directory.
RPFITSROOT := ..

# Only used for locating architecture-specific code in ../code/$(RPARCH)
# (../code/darwin_x86 is currently symlink'd to ../code/linux).
RPARCH := darwin_x86

# GNU Fortran 77 and C compiler flags.  
FC      := g77
FFLAGS  := -g -O -fno-automatic -Wall -Wimplicit -Wno-globals
CC      := gcc
CFLAGS  := -g -O -Wall

# Use these definitions (in place of those above) to compile with gfortran.
# FC      := gfortran
# FFLAGS  := -g -O -fno-automatic -Wall -Wimplicit
# LDFLAGS := -lg2c

RANLIB  := /usr/bin/ranlib

# Where to install stuff.
PREFIX := /usr/local

#-----------------------------------------------------------------------------
# Include generic RPFITS makefile.
-include $(RPFITSROOT)/GNUmakefile
