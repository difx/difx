# Linux Makefile for RPFITS.

# The RPFITS root directory.
RPFITSROOT := ..

# Only used for locating architecture-specific code in ./code/$(ARCH).
ARCH := linux

# GNU Fortran and C compiler flags.
FC     := g77
FFLAGS := -g -O -fno-automatic -Wall -Wno-globals
CFLAGS := -g -O -Wall -Wno-parentheses
RANLIB := ranlib

LDFLAGS :=

# Use these definitions (in place of those above) to compile with gfortran.
# FC      := gfortran
# FFLAGS  := -g -O -fno-automatic -Wall
# LDFLAGS := -lg2c

# Where to install stuff.
PREFIX := /usr/local

#-----------------------------------------------------------------------------
# Include generic RPFITS makefile.
-include $(RPFITSROOT)/GNUmakefile
