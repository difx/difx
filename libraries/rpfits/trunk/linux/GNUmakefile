# Linux Makefile for RPFITS (grus, Debian etch).

# The RPFITS root directory.
RPFITSROOT := ..

# Only used for locating architecture-specific code in ./code/$(RPARCH).
RPARCH := linux

# C compiler flags.  
CC      := gcc
CFLAGS  := -g -O -Wall

# GNU Fortran 77
ifdef USEGFORTRAN
  FC      := gfortran
  FFLAGS  := -g -O -fno-automatic -Wall -Wimplicit
  LDFLAGS := -lg2c
else
  FC      := g77
  FFLAGS  := -g -O -fno-automatic -Wall -Wimplicit -Wno-globals
  LDFLAGS :=
endif
RANLIB := ranlib

# Where to install stuff.
PREFIX := /usr/local

#-----------------------------------------------------------------------------
# Include generic RPFITS makefile.
-include $(RPFITSROOT)/GNUmakefile
