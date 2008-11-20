# Linux Makefile for RPFITS (delphinus, Debian etch).

# The RPFITS root directory.
RPFITSROOT := ..

# Only used for locating architecture-specific code in ./code/$(RPARCH).
RPARCH := linux

# GNU Fortran and C compiler flags.
FC     := g77
FFLAGS := -g -O -fno-automatic -Wall -Wimplicit -Wno-globals
CFLAGS := -g -O -Wall
RANLIB := ranlib

LDFLAGS :=

# Use these definitions (in place of those above) to compile with gfortran.
# FC      := gfortran
# FFLAGS  := -g -O -fno-automatic -Wall -Wimplicit
# LDFLAGS := -lg2c

# Where to install stuff.
PREFIX := /usr/local

#-----------------------------------------------------------------------------
# Include generic RPFITS makefile.
-include $(RPFITSROOT)/GNUmakefile
